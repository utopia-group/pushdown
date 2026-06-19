import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object UserDefinedAggregationFunctionTest_2 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UserDefinedAggregationFunctionTest_2")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("user", IntegerType, nullable = false),
      StructField("time", LongType, nullable = false)
    ))

    val udaf = new SessionDurationAggregator()

    val predicate = $"time" <= 3 || $"time" >= 58

		val parquetPath = "UserDefinedAggregationFunctionTest_16.parquet"
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
              "user" -> Zipf(1_000_000, 1.25),
              // "time" -> Gaussian(50.0, 20.0)
              "time" -> Zipf(60, 1.35)),
            seed = Some(42L)
          )
          // .withColumn("time", $"time" + 20)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"user").agg(udaf($"time").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"first_log_time".isNull && $"first_log_time" <= 3 &&
      !$"last_log_time".isNull && $"last_log_time" >= 58)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"user").agg(udaf($"time").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"first_log_time" <= 3 &&
      $"last_log_time" >= 58)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
