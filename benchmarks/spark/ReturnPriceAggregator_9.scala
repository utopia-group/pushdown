import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object ReturnPriceAggregator_9 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("ReturnPriceAggregator_9")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new ReturnPriceAggregator()

    val schema = StructType(Seq(
      StructField("symbol", IntegerType, nullable = false),
      StructField("price", DoubleType, nullable = false),
      StructField("epoch", LongType  , nullable = false)
    ))

    val predicate = $"epoch" <= 40 || $"epoch" >= 53

		val parquetPath = "ReturnPriceAggregator_12.parquet"
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
              "symbol" -> Zipf(50_000, 1.02),
              "price" -> Zipf(100, 1.15),
              "epoch" -> Zipf(25, 1.01)),
            seed = Some(42L)
          )
          .withColumn("symbol", format_string("s%04d", $"symbol"))
          .withColumn("epoch", $"epoch" + 36)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"symbol").agg(udaf($"price", $"epoch").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      (($"openPrice" > -5 && $"openPrice" < 115) ||
       ($"closePrice" >= 0 && $"closePrice" <= 100)) &&
      !$"openEpoch".isNull && ($"openEpoch" <= 38 || $"openEpoch" === 40) &&
      !$"closeEpoch".isNull && ($"closeEpoch" >= 55 || $"closeEpoch" === 53))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"symbol").agg(udaf($"price", $"epoch").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      (($"openPrice" > -5 && $"openPrice" < 115) ||
       ($"closePrice" >= 0 && $"closePrice" <= 100)) &&
      !$"openEpoch".isNull && ($"openEpoch" <= 38 || $"openEpoch" === 40) &&
      !$"closeEpoch".isNull && ($"closeEpoch" >= 55 || $"closeEpoch" === 53))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
