// https://github.com/csruiliu/rotary-aqp/blob/d8032a3f722fe538aa09cd2e686ccfe2204736a3/aqp/src/main/scala/ruiliu/aqp/tpch/TPCHUDAF.scala#L557
// https://github.com/csruiliu/rotary-aqp/blob/d8032a3f722fe538aa09cd2e686ccfe2204736a3/aqp/src/main/scala/ruiliu/aqp/tpch/QueryTPCH.scala#L669
// https://github.com/csruiliu/rotary-aqp/blob/d8032a3f722fe538aa09cd2e686ccfe2204736a3/aqp/src/main/scala/ruiliu/aqp/tpch/CardTPCH.scala#L473

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class UDAF_Q12_HIGH extends UserDefinedAggregateFunction {
  var startTime: Long = System.currentTimeMillis()
  var currentTime: Long = startTime

  var aggregationSchemaName: String = _
  var aggregationInterval: Int = 0

  def setAggregationInterval(inputInterval: Int): Unit = {
    this.aggregationInterval = inputInterval
  }

  def setAggregationSchemaName(inputSchemaName: String): Unit = {
    this.aggregationSchemaName = inputSchemaName
  }

  // This is the input fields for your aggregate function.
  override def inputSchema: StructType = StructType(StructField("o_orderpriority", StringType) :: Nil)

  // This is the internal fields you keep for computing your aggregate.
  override def bufferSchema: StructType = StructType(StructField("count", LongType) :: Nil)

  // This is the output type of your aggregatation function.
  override def dataType: DataType = LongType

  override def deterministic: Boolean = true

  // This is the initial value for your buffer schema.
  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = 0L
  }

  // This is how to update your buffer schema given an input.
  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val priority = input.getAs[String](0)

    if (priority == "1-URGENT" || priority == "2-HIGH") {
      buffer(0) = buffer.getAs[Long](0) + 1
    }

    this.currentTime = System.currentTimeMillis()
    if (this.aggregationInterval != 0 && this.currentTime - this.startTime > this.aggregationInterval) {
      println("Aggregation|%s|%d|%d|%d".format(
        this.aggregationSchemaName, buffer.getLong(0), this.startTime, this.currentTime)
      )
      this.startTime = System.currentTimeMillis()
    }

  }

  // This is how to merge two objects with the bufferSchema type.
  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    buffer1(0) = buffer1.getAs[Long](0) + buffer2.getAs[Long](0)
  }

  // This is where you output the final value, given the final value of your bufferSchema.
  override def evaluate(buffer: Row): Any = {
    buffer.getLong(0)
  }
}

object TPCHUDAF_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("TPCHUDAF_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("l_shipmode", IntegerType, nullable = false),
      StructField("o_orderpriority", IntegerType, nullable = false)
    ))

    val udaf = new UDAF_Q12_HIGH()

    val predicate = $"o_orderpriority" === "1-URGENT" || $"o_orderpriority" === "2-HIGH"

    val shipModes = Array("MAIL", "RAIL", "TRUCK", "SHIP", "AIR")
    val orderPriorities = Array("1-URGENT", "2-HIGH", "3-MEDIUM", "4-LOW")

		val parquetPath = "TPCHUDAF_12.parquet"
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
              "l_shipmode" -> Zipf(shipModes.size, 1.50),
              "o_orderpriority" -> Zipf(orderPriorities.size, 1.15)),
            seed = Some(42L)
          )
          .withColumn("l_shipmode", udf((i: Int) => shipModes(i - 1)).apply($"l_shipmode"))
          .withColumn("o_orderpriority", udf((i: Int) => orderPriorities(i - 1)).apply($"o_orderpriority"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"l_shipmode").agg(udaf($"o_orderpriority").as("row"))
    val filtered = out.filter(
      ($"row" > 10 && $"row" =!= 19) ||
      ($"row" < 5 && $"row" =!= 3))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"l_shipmode").agg(udaf($"o_orderpriority").as("row"))
    val filtered_pre = out_pre.filter(
      ($"row" > 10 && $"row" =!= 19) ||
      ($"row" < 5 && $"row" =!= 3))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
