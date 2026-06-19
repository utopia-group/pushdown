import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object EventAggregationUdf_5 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("EventAggregationUdf_5")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new EventAggregationUdf()

    val schema = StructType(Seq(
      StructField("ticker", IntegerType),
      StructField("action", IntegerType),
      StructField("eventTime", TimestampType)))

    val left = to_timestamp(lit("1980-07-30 08:00:00"), "yyyy-MM-dd HH:mm:ss")
    val right = to_timestamp(lit("2001-07-30 08:00:00"), "yyyy-MM-dd HH:mm:ss")

    val predicate =
      $"eventTime" < left ||
      $"eventTime" >= right ||
      $"action" === "a" ||
      $"action" === "b"

    val actionMap = array(lit("time"), lit("price"), lit("click"), lit("a"), lit("b"))
    val baseEpoch = unix_timestamp(lit("1980-07-30 08:00:00")).cast("bigint")

		val parquetPath = "EventAggregationUdf_11.parquet"
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
              "ticker" -> Zipf(5_000, 1.10),
              "action" -> Zipf(5, 2.5),
              "eventTime" -> Zipf(662_000_000, 1.20)),
            seed = Some(42L))
          .withColumn("action", element_at(actionMap, $"action"))
          .withColumn("eventTime",
            from_unixtime($"eventTime".cast("bigint") + baseEpoch).cast("timestamp"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"ticker").agg(udaf($"action", $"eventTime").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      $"time" > 7 && $"price" > 18 &&
      !$"firstEvent".isNull && $"firstEvent" < left &&
      !$"lastEvent".isNull && $"lastEvent" >= right)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9

    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"ticker").agg(udaf($"action", $"eventTime").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"lastEvent" >= right &&
      $"price" > 18 &&
      $"firstEvent" < left &&
      $"time" > 7)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${runtime_pre}%.2f")

    spark.stop()
  }
}
