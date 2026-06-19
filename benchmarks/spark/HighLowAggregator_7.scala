import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import java.math.{BigDecimal => JBigDecimal}
import java.sql.Timestamp
import org.joda.time.LocalDateTime

object HighLowAggregator_7 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("HighLowAggregator_7")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new HighLowAggregator()

    val schema = StructType(Seq(
   		StructField("day", IntegerType),
   		StructField("hour", IntegerType),
   		StructField("min", IntegerType),
   		StructField("sec", IntegerType),
   		StructField("milli", IntegerType),
   		StructField("tradHour", IntegerType),
   		StructField("tradMin", IntegerType),
   		StructField("price", new DecimalType(12,6)),
   		StructField("thresh", IntegerType)))

    val getNfpTsUDF = udf(udaf.getNfpTs _)
    val getTickTsUDF = udf(udaf.getTickTs _)
    val tsPlusMinsUDF = udf(udaf.tsPlusMins _)

		val nfpTs = getNfpTsUDF($"day", $"tradHour", $"tradMin")
		val tickTs = getTickTsUDF($"day", $"hour", $"min", $"sec", $"milli")

    val predicate = tickTs > nfpTs && tickTs < tsPlusMinsUDF(nfpTs, $"thresh")

		val parquetPath = "HighLowAggregator_12.parquet"
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
							"day" -> Zipf(365, 1.05),     
							"hour" -> Zipf(24, 1.02),              
							"min" -> Zipf(60, 1.02),              
							"sec" -> Zipf(60, 1.02),              
							"milli" -> Zipf(1000, 1.02),            
							"tradHour" -> Zipf(24, 3.0),
							"tradMin" -> Zipf(60, 3.0),
							"price" -> Zipf(100, 1.25),
							"thresh" -> Zipf(60, 2.5)),
            seed = Some(42L))
					.withColumn(
						"day",
						date_format(date_sub(lit("2020-12-31"), $"day" - 1), "yyyyMMdd")
					)
					.withColumn("hour", ($"hour" + 10) % 24)
					.withColumn("min", ($"min" + 12) % 60)
					.withColumn("sec", $"sec" - 1)
					.withColumn("milli", $"milli" - 1)
					.withColumn("tradHour", $"tradHour" - 1)
					.withColumn("tradMin", $"tradMin" - 1)
					.withColumn("price", ($"price" - 1).cast("decimal(12,6)"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"thresh").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded = out.select($"row.*")
    val ts1 = to_timestamp(lit("2020-01-30 08:00:00"), "yyyy-MM-dd HH:mm:ss")
    val ts2 = to_timestamp(lit("2020-06-30 08:00:00"), "yyyy-MM-dd HH:mm:ss")
    val filtered = exploded.filter(
			($"high_price".isNull || $"high_price" === 99 ||
			 $"low_price".isNull || $"low_price" <= 50) &&
			($"high_price_ts".isNull || $"high_price_ts" > ts1) &&
			($"low_price_ts".isNull || $"low_price_ts" =!= ts2))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"thresh").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
			($"high_price".isNull || $"high_price" === 99 ||
			 $"low_price".isNull || $"low_price" <= 50) &&
			($"high_price_ts".isNull || $"high_price_ts" > ts1) &&
			($"low_price_ts".isNull || $"low_price_ts" =!= ts2))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
