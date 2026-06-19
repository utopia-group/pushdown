// https://github.com/QuantLet/Cryptocurrencies-and-Stablecoins-a-high-frequency-analysis/blob/main/src/main/scala/Aggregators/ReturnPriceAggregator.scala
// https://github.com/QuantLet/Cryptocurrencies-and-Stablecoins-a-high-frequency-analysis/blob/859a57509b57b5809ee2e0c025a1efbdf8ba2a49/src/main/scala/Utilities.scala#L7

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class ReturnPriceAggregator extends UserDefinedAggregateFunction {

  override def inputSchema: StructType = StructType(Seq(
    StructField("price", DoubleType, nullable = false),   // arg 0
    StructField("epoch", LongType  , nullable = false)    // arg 1
  ))

  override def bufferSchema: StructType = StructType(Seq(
    StructField("openPrice" , DoubleType, nullable = false),
    StructField("openEpoch" , LongType  , nullable = true),
    StructField("closePrice", DoubleType, nullable = false),
    StructField("closeEpoch", LongType  , nullable = true)
  ))

  override def dataType: DataType = bufferSchema
  override def deterministic: Boolean = true

  override def initialize(buf: MutableAggregationBuffer): Unit = {
    buf(0) = 0.0
    buf(1) = null
    buf(2) = 0.0
    buf(3) = null
  }

  override def update(buf: MutableAggregationBuffer, row: Row): Unit = {
    val price = row.getDouble(0)
    val time  = row.getLong(1)

    if (buf.isNullAt(1) || time < buf.getLong(1)) {
      buf(0) = price
      buf(1) = time
    }

    if (buf.isNullAt(3) || time > buf.getLong(3)) {
      buf(2) = price
      buf(3) = time
    }
  }

  override def merge(b1: MutableAggregationBuffer, b2: Row): Unit = {
    if (!b2.isNullAt(1) && (b1.isNullAt(1) || b2.getLong(1) < b1.getLong(1))) {
      b1(0) = b2.getDouble(0)
      b1(1) = b2.getLong(1)
    }

    if (!b2.isNullAt(3) && (b1.isNullAt(3) || b2.getLong(3) > b1.getLong(3))) {
      b1(2) = b2.getDouble(2)
      b1(3) = b2.getLong(3)
    }
  }

  override def evaluate(buf: Row): Any = buf
}

object ReturnPriceAggregator_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("ReturnPriceAggregator_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new ReturnPriceAggregator()

    val schema = StructType(Seq(
      StructField("symbol", IntegerType, nullable = false),
      StructField("price", DoubleType, nullable = false),
      StructField("epoch", LongType, nullable = false)
    ))

    val predicate = $"epoch" < 38 || $"epoch" > 55

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
      $"openPrice" =!= 0 &&
      !$"openEpoch".isNull &&
      $"openEpoch" < 38 &&
      $"closePrice" =!= 0 &&
      !$"closeEpoch".isNull &&
      $"closeEpoch" > 55)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"symbol").agg(udaf($"price", $"epoch").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      ($"closeEpoch".isNull || $"closeEpoch" > 55) &&
      ($"openEpoch".isNull || $"openEpoch" < 38) &&
      $"openPrice" =!= 0 &&
      $"closePrice" =!= 0)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
