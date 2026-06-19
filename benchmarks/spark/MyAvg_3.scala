import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import scala.util.Random

object MyAvg_3 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("MyAvg_3")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new MyAvg()

    val schema = StructType(Seq(
      StructField("gender", StringType, nullable = false),
      StructField("inputDouble_num", IntegerType, nullable = true)))

		val predicate = !$"inputDouble".isNull
		
		val parquetPath = "MyAvg_12.parquet"
    val fs          = FileSystem.get(spark.sparkContext.hadoopConfiguration)
    val pathObj     = new Path(parquetPath)
		val df =
      if (fs.exists(pathObj)) {
        // println(s"⏩  Using cached data at $parquetPath")
        spark.read.parquet(parquetPath)
      } else {
        // println(s"🔄  Generating fresh input and writing to $parquetPath")
        DataSynth
          .generate(
            spark,
            schema,
            rows = 100_000,
            distributions = Map(
              "gender" -> CategoricalString.uniform("female", "male"),
              "inputDouble_num" -> Zipf(100_001, 1.2)),
            seed = Some(42L))
          .withColumn(
            "inputDouble",
            when($"inputDouble_num".between(2, 6), lit(null).cast(DoubleType))
              .otherwise($"inputDouble_num".cast(DoubleType))
          )
          .drop("inputDouble_num")
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"gender").agg(udaf($"inputDouble").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      (!$"bufferSum".isNull && $"bufferSum" > 150000 && $"bufferSum" =!= 175000 && $"bufferSum" < 200000) ||
      $"bufferCount" > 0)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9

    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy("gender").agg(udaf($"inputDouble").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter($"bufferSum".isNotNull || $"bufferCount" > 0)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${runtime_pre}%.2f")

    spark.stop()
  }
}
