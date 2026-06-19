// https://github.com/opencypher/morpheus/blob/f54768bab48cab4ae1cf8e2030420fedd9ab18d3/morpheus-spark-cypher/src/main/scala/org/opencypher/morpheus/impl/temporal/TemporalUdafs.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.spark.unsafe.types.CalendarInterval

abstract class SimpleDurationAggregation(aggrName: String) extends UserDefinedAggregateFunction {
  override def inputSchema: StructType = StructType(Array(StructField("duration", CalendarIntervalType)))
  override def bufferSchema: StructType = StructType(Array(StructField(aggrName, CalendarIntervalType)))
  override def dataType: DataType = CalendarIntervalType
  override def deterministic: Boolean = true
  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = new CalendarInterval(0, 0, 0L)
  }
  override def evaluate(buffer: Row): Any = buffer.getAs[CalendarInterval](0)
}

class DurationMin extends SimpleDurationAggregation("min") {
  override def bufferSchema: StructType = StructType(Seq(
    StructField("months", IntegerType, nullable = true),
    StructField("days", IntegerType, nullable = true),
    StructField("micros", LongType, nullable = true)
  ))
  private val µPerDay   = 24L * 3_600_000_000L
  private val µPerMonth = 30L * µPerDay
  def micros(i: CalendarInterval): Long =
    i.months.toLong * µPerMonth + i.days.toLong * µPerDay + i.microseconds
  override def initialize(buf: MutableAggregationBuffer): Unit = {
    buf(0) = null
    buf(1) = null
    buf(2) = null
  }
  override def update(buf: MutableAggregationBuffer, input: Row): Unit = {
    val candidate = input.getAs[CalendarInterval](0)

    if (
      (buf.isNullAt(0) && buf.isNullAt(1) && buf.isNullAt(2)) ||
      micros(candidate) < micros(new CalendarInterval(buf.getInt(0), buf.getInt(1), buf.getLong(2)))) {
      buf(0) = candidate.months
      buf(1) = candidate.days
      buf(2) = candidate.microseconds
    }
  }
  override def merge(b1: MutableAggregationBuffer, b2: Row): Unit = {
    val i2 = new CalendarInterval(b2.getInt(0), b2.getInt(1), b2.getLong(2))

    if (
      (b1.isNullAt(0) && b1.isNullAt(1) && b1.isNullAt(2)) ||
      micros(i2) < micros(new CalendarInterval(b1.getInt(0), b1.getInt(1), b1.getLong(2)))) {
      b1(0) = i2.months
      b1(1) = i2.days
      b1(2) = i2.microseconds
    }
  }
  override def evaluate(buf: Row): Any =
    new CalendarInterval(buf.getInt(0), buf.getInt(1), buf.getLong(2))
}

object DurationMin_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("DurationMin_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("key", IntegerType, nullable = false),
      StructField("months", IntegerType, nullable = false),
      StructField("days", IntegerType, nullable = false),
      StructField("micros", LongType, nullable = false)))

    val udaf = new DurationMin()

    val monthsUDF = udf((ci: CalendarInterval) => ci.months)
    val daysUDF = udf((ci: CalendarInterval) => ci.days)
    val microsUDF = udf((ci: CalendarInterval) => ci.microseconds)

    val predicate = daysUDF($"duration") < 60

    val toCal = udf((m: Int, d: Int, mi: Int) => new CalendarInterval(m, d, mi))

		val parquetPath = "DurationMin_11.parquet"
    val fs          = FileSystem.get(spark.sparkContext.hadoopConfiguration)
    val pathObj     = new Path(parquetPath)
		var df =
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
              "key" -> Zipf(1_000_000, 1.15),
              "months" -> Zipf(50, 1.40),
              "days" -> Zipf(30, 1.40),
              "micros" -> Zipf(100, 1.40)),
            seed = Some(42L))
          .withColumn("months", $"months" + 10)
          .withColumn("days", $"days" + 40)
          .withColumn("micros", $"micros" + 60)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
    df = df
      .withColumn("duration", toCal($"months", $"days", $"micros"))
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"key").agg(udaf($"duration").as("row"))
    val filtered = out.filter(
      !$"row".isNull &&
      daysUDF($"row") > 10 &&
      daysUDF($"row") =!= 35 &&
      daysUDF($"row") < 60)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    // println(spark.range(1).select(daysUDF(lit(null))).collect()(0)(0))

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"key").agg(udaf($"duration").as("row"))
    val filtered_pre = out_pre.filter(
      $"row".isNotNull &&
      daysUDF($"row") =!= 35 &&
      daysUDF($"row") > 10)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
