// https://github.com/bartosz25/spark-scala-playground/blob/d4dae02098169e9f4241f3597dc4864421237881/src/test/scala/com/waitingforcode/sql/UserDefinedAggregationFunctionTest.scala#L94

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class SessionDurationAggregator extends UserDefinedAggregateFunction {

  override def inputSchema: StructType =
    StructType(Seq(StructField("time", LongType, nullable = false)))

  override def bufferSchema: StructType = StructType(Seq(
    StructField("first_log_time", LongType, nullable = true),
    StructField("last_log_time", LongType, nullable = true)
  ))

  override def dataType: DataType = bufferSchema

  override def deterministic: Boolean = true

  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, null)
    buffer.update(1, null)
  }

  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val logTime = input.getLong(0)
    if (buffer.isNullAt(0) || logTime < buffer.getLong(0)) {
      buffer.update(0, logTime)
    }
    if (buffer.isNullAt(1) || logTime > buffer.getLong(1)) {
      buffer.update(1, logTime)
    }
  }

  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    if (buffer1.isNullAt(0) || (!buffer2.isNullAt(0) && buffer2.getLong(0) < buffer1.getLong(0))) {
      buffer1.update(0, buffer2.get(0))
    }
    if (buffer1.isNullAt(1) || (!buffer2.isNullAt(1) && buffer2.getLong(1) > buffer1.getLong(1))) {
      buffer1.update(1, buffer2.get(1))
    }
  }

  override def evaluate(buffer: Row): Any = buffer
}

object UserDefinedAggregationFunctionTest_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UserDefinedAggregationFunctionTest_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("user", IntegerType, nullable = false),
      StructField("time", LongType, nullable = false)
    ))

    val udaf = new SessionDurationAggregator()

    val predicate = $"time" <= 5 || $"time" >= 57

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
      !$"first_log_time".isNull &&
      ($"first_log_time" <= 3 || $"first_log_time" === 5) &&
      !$"last_log_time".isNull &&
      ($"last_log_time" === 57 || $"last_log_time" >= 59))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"user").agg(udaf($"time").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      ($"first_log_time" <= 3 || $"first_log_time" === 5) &&
      ($"last_log_time" === 57 || $"last_log_time" >= 59))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
