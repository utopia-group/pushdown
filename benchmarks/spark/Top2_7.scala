import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
// import org.apache.spark.sql.catalyst.expressions.GenericRowWithSchema
import org.apache.hadoop.fs.{FileSystem, Path}
import java.util.Comparator
// import scala.collection.mutable

object Top2_7 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("Top2_7")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("op1", StringType, nullable = false),
      StructField("value", LongType, nullable = false)
    ))

    val udaf = new TopKUDAF[Long](2, schema, "value")

    val predicate = $"value" >= 80

    val op1Values = Array("ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT", "GOLF", "HOTEL", "INDIA", "JULIET")

		val parquetPath = "Top2_12.parquet"
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
            rows = 100_000_000,
            distributions = Map(
              "op1" -> Zipf(op1Values.size, 1.50),
              // "value" -> Gaussian(90.0, 10.0)
              "value" -> Zipf(100, 1.20)
            ),
            seed = Some(42L)
          )
          .withColumn("op1", udf((i: Int) => op1Values(i - 1)).apply($"op1"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"op1").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"arrData".getItem(0).isNull &&
      ($"arrData".getItem(0).getField("value") === 80 ||
       $"arrData".getItem(0).getField("value") >= 90) &&
      !$"arrData".getItem(1).isNull &&
      $"arrData".getItem(1).getField("value") >= 80 &&
      $"arrData".getItem(1).getField("value") =!= 90)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"op1").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      ($"arrData".getItem(0).isNull ||
       $"arrData".getItem(0).getField("value") === 80 ||
       $"arrData".getItem(0).getField("value") >= 90) &&
      $"arrData".getItem(1).getField("value") =!= 90)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
