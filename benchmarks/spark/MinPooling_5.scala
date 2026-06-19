import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import scala.util.Random

object MinPooling_5 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("MinPooling_5")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new MinPoolingUDAF()

    val schema = StructType(Seq(
      StructField("anchor", IntegerType, nullable = false),
      StructField("value", ArrayType(DoubleType, false), nullable = true)))

		val predicate =
      !$"value".getItem(0).isNull &&
      !$"value".getItem(1).isNull &&
      !$"value".getItem(2).isNull

    val valueCol: Column =
      when(rand() < 0.40, lit(null).cast(ArrayType(DoubleType, containsNull = false)))
        .otherwise(
          array(
            (0 until udaf.embeddingSize).map { _ =>
              randn() * 1.0 + 13.0        // μ = 13, σ = 1
            }: _*
          )
        )

		val parquetPath = "MinPooling_11.parquet"
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
              "anchor" -> Zipf(450_000, 1.01)),  // 450k groups for ~50% selectivity
            overrides = Map(
              "value" -> valueCol),
            seed = Some(42L))
          .withColumn("anchor", format_string("id_%04d", $"anchor"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"anchor").agg(udaf($"value").as("row"))
    val filtered = out.filter(
      ($"row".getItem(0).isNull || $"row".getItem(0) === 6) &&
      ($"row".getItem(1).isNull || $"row".getItem(1) >= 6) &&
      ($"row".getItem(2).isNull || $"row".getItem(2) < 29))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy("anchor").agg(udaf($"value").as("row"))
    val filtered_pre = out_pre.filter(
      ($"row".getItem(0).isNull || $"row".getItem(0) === 6) &&
      ($"row".getItem(1).isNull || $"row".getItem(1) >= 6) &&
      ($"row".getItem(2).isNull || $"row".getItem(2) < 29))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
