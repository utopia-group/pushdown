// https://github.com/okmich/fx-usd-non-farm-payroll/blob/f54a3bad1c597b0b00d0d14096753de47ba2e833/nfp-fx/src/main/scala/udaf/HighLowAggregator.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import java.math.{BigDecimal => JBigDecimal}
import java.sql.Timestamp
import org.joda.time.LocalDateTime

class HighLowAggregator extends UserDefinedAggregateFunction {

	type Price = (JBigDecimal, Timestamp, JBigDecimal, Timestamp)

	override def inputSchema: StructType = StructType(
   		StructField("day", StringType) ::
   		StructField("hour", IntegerType) ::
   		StructField("min", IntegerType) ::
   		StructField("sec", IntegerType) ::
   		StructField("milli", IntegerType) ::
   		StructField("tradHour", IntegerType) ::
   		StructField("tradMin", IntegerType) ::
   		StructField("price", new DecimalType(12,6)) ::
   		StructField("thresh", IntegerType) :: Nil)

   	override def bufferSchema: StructType = StructType(
	    StructField("hPrice", new DecimalType(12,6), nullable = true) ::
	    StructField("hPriceTs", TimestampType, nullable = true) ::
	    StructField("lPrice", new DecimalType(12,6), nullable = true) ::
	    StructField("lPriceTs", TimestampType, nullable = true) :: Nil)

   	override def dataType: DataType = StructType(
	    StructField("high_price", new DecimalType(12,6), nullable = true) ::
	    StructField("high_price_ts", TimestampType, nullable = true) ::
	    StructField("low_price", new DecimalType(12,6), nullable = true) ::
	    StructField("low_price_ts", TimestampType, nullable = true) :: Nil)

	override def initialize(buffer: MutableAggregationBuffer): Unit = {
	    buffer(0) = null
	    buffer(1) = null
	    buffer(2) = null
	    buffer(3) = null
	  }

	override def deterministic : Boolean = true

	// This is how to update your buffer schema given an input.
	override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
		val day = input.getAs[String](0)
		val hour = input.getAs[Int](1)
		val min = input.getAs[Int](2)
		val sec = input.getAs[Int](3)
		val milli = input.getAs[Int](4)
		val tradHour = input.getAs[Int](5)
		val tradMin = input.getAs[Int](6)
		val price = input.getAs[JBigDecimal](7)
		val x = input.getAs[Int](8)

		val nfpTs = getNfpTs(day, tradHour, tradMin)
		val tickTs = getTickTs(day, hour, min, sec, milli)
		//if the tickTs is between nfpTs and nfpTsPlusThresh
		//big filtering 
		if (tickTs.after(nfpTs) && tickTs.before(tsPlusMins(nfpTs, x))) {
			//// println(s"nfpTs is $nfpTs while tickTs is $tickTs")
			buffer(0) = price
			buffer(1) = tickTs
			buffer(2) = price
			buffer(3) = tickTs
		}
	}

	override def merge(buffer: MutableAggregationBuffer, row: Row): Unit = {
		val result = getEarliestAndLatest(buffer, row)

		buffer(0) = result._1
		buffer(1) = result._2
		buffer(2) = result._3
		buffer(3) = result._4
	}

	// This is where you output the final value, given the final value of your bufferSchema.
	override def evaluate(buffer: Row): Any = {
		(buffer(0), buffer(1), buffer(2), buffer(3))
	}

	def getTickTs(day: String, hour: Int, min: Int, sec: Int, milli:Int) : Timestamp  = {
		new Timestamp(new LocalDateTime(
			day.substring(0,4).toInt,
			day.substring(4,6).toInt,
			day.substring(6,8).toInt,
			hour, min, sec, milli
		).toDate.getTime)
	}

	def getNfpTs(day: String, nfpHour: Int, nfpMin: Int) : Timestamp = {
		new Timestamp(new LocalDateTime(
			day.substring(0,4).toInt,
			day.substring(4,6).toInt,
			day.substring(6,8).toInt,
			nfpHour, nfpMin, 0, 0
		).toDate.getTime)
	}	

	def tsPlusMins(ts: Timestamp, thresh: Int) : Timestamp = {
		val localDT = new LocalDateTime(ts.getTime).plusMinutes(thresh)

		new Timestamp(localDT.toDate.getTime)
	}	

	def getEarliestAndLatest(a : Price, b: Price) : Price = {
		var open : (JBigDecimal, Timestamp) = null
		var close : (JBigDecimal, Timestamp) = null

		if (b._2 != null){
			//get min time
			open = if (a._2 == null || b._2.before(a._2)) (b._1, b._2) else (a._1, a._2)
			//get the max time
			close = if (a._2 == null || b._4.after(a._4)) (b._3, b._4) else (a._3, a._4)
		} else {
			open = (a._1, a._2)
			close = (a._3, a._4)
		}

		(open._1, open._2, close._1, close._2)
	}

	implicit def castInternalBufferToPrice(buffer: MutableAggregationBuffer) : Price = {
		(buffer.getAs[JBigDecimal](0), buffer.getAs[Timestamp](1),
			 buffer.getAs[JBigDecimal](2), buffer.getAs[Timestamp](3))
	}

	implicit def castInternalBufferToPrice(row: Row) : Price = {
		(row.getAs[JBigDecimal](0), row.getAs[Timestamp](1), 
			row.getAs[JBigDecimal](2), row.getAs[Timestamp](3))
	}
}

object HighLowAggregator_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("HighLowAggregator_1")
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
    val ts = to_timestamp(lit("2020-07-30 08:00:00"), "yyyy-MM-dd HH:mm:ss")
    val filtered = exploded.filter(
			(!$"high_price".isNull && $"high_price" < 10) ||
			(!$"high_price_ts".isNull && $"high_price_ts" >= ts) ||
			(!$"low_price".isNull && $"low_price" <= 99))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"thresh").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
			$"high_price" < 10 ||
			$"high_price_ts" >= ts ||
			$"low_price" <= 99)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
