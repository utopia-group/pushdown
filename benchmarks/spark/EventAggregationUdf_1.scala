// https://github.com/Nemchinovrp/Fintech-Trading/blob/66df1f457d47c0bdaffb15ee21115f3d8d57ce33/spark-processor2/src/main/scala/me/sandbox/sql/streaming/spark/udf/EventAggregationUdf.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class EventAggregationUdf extends UserDefinedAggregateFunction {

  override def inputSchema: StructType =
    StructType(
      StructField("action", StringType) ::
        StructField("eventTime", TimestampType) ::
        Nil)

  override def bufferSchema: StructType =
    StructType(
      StructField("time", LongType) ::
        StructField("price", LongType) ::
        StructField("firstEvent", TimestampType) ::
        StructField("lastEvent", TimestampType) ::
        Nil)

  override def dataType: DataType = bufferSchema

  override def deterministic: Boolean = true

  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = 0L
    buffer(1) = 0L
  }

  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    /* aggregate count clicks and watches */
    if (input(0) == "time") {
      buffer(0) = buffer.getLong(0) + 1L
    } else if (input(0) == "price") {
      buffer(1) = buffer.getLong(1) + 1L
    } else {}

    val actionEvent = input.getTimestamp(1)
    if (!buffer.isNullAt(2) && !buffer.isNullAt(3)) {
      /* firstEvent min actionEvent */
      if (actionEvent.before(buffer.getTimestamp(2))) buffer(2) = actionEvent

      /* lastEvent max actionEvent */
      if (actionEvent.after(buffer.getTimestamp(3))) buffer(3) = actionEvent
    } else {
      buffer(2) = actionEvent
      buffer(3) = actionEvent
    }
  }

  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    /* aggregate count clicks and watches */
    buffer1(0) = buffer1.getLong(0) + buffer2.getLong(0)
    buffer1(1) = buffer1.getLong(1) + buffer2.getLong(1)

    /* firstEvent is min or nothing changes */
    if (!buffer1.isNullAt(2) && !buffer2.isNullAt(2)) {
      val l = buffer1.getTimestamp(2)
      val r = buffer2.getTimestamp(2)
      if (r.before(l)) {
        buffer1(2) = r
      }
    } else if (!buffer2.isNullAt(2)) {
      buffer1(2) = buffer2.getTimestamp(2)
    }

    /* lastEvent is max or nothing changes */
    if (!buffer1.isNullAt(3) && !buffer2.isNullAt(3)) {
      val l = buffer1.getTimestamp(3)
      val r = buffer2.getTimestamp(3)
      if (r.after(l)) {
        buffer1(3) = r
      }
    } else if (!buffer2.isNullAt(3)) {
      buffer1(3) = buffer2.getTimestamp(3)
    }
  }

  override def evaluate(buffer: Row): Any = buffer
}

object EventAggregationUdf_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("EventAggregationUdf_1")
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
      $"eventTime" > right ||
      $"action" === "a" ||
      $"action" === "b"

    val actionMap = Array("time", "price", "click", "a", "b")
    val baseEpoch = unix_timestamp(lit("1980-07-25 08:00:00")).cast("bigint")

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
            // rows = 100_000,
            rows = 100_000_000,
						distributions = Map(
              "ticker" -> Zipf(5_000, 2.0),
              "action" -> Zipf(actionMap.size, 2.5),
              "eventTime" -> Zipf(7_800, 1.30)),
            seed = Some(42L))
          .withColumn("action", udf((index: Int) => actionMap(index - 1)).apply($"action"))
          .withColumn("eventTime",
            from_unixtime($"eventTime".cast("bigint") * 86_400 + baseEpoch).cast("timestamp"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"ticker").agg(udaf($"action", $"eventTime").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"firstEvent".isNull && $"firstEvent" < left &&
      !$"lastEvent".isNull && $"lastEvent" > right)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9

    // exploded.show(10, truncate = false)
    // filtered.show(10, truncate = false)


    val t0_pre = System.nanoTime()

    // println(spark.range(1).select(lit(null).cast(TimestampType) <= left).collect()(0)(0))

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"ticker").agg(udaf($"action", $"eventTime").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"lastEvent" > right &&
      $"firstEvent" < left)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
