// https://github.com/fybrik/mover/blob/8393b3cc6abf08dd63d62c71fb77e17b57fccb5d/src/main/scala/io/fybrik/mover/spark/SnapshotAggregator.scala#L19
// https://github.com/fybrik/mover/blob/8393b3cc6abf08dd63d62c71fb77e17b57fccb5d/src/test/scala/io/fybrik/mover/spark/SnapshotSuite.scala#L40

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

/**
  * This aggregator compares values of the same key and returns only the one with the largest offset value.
  */
class SnapshotAggregator() extends UserDefinedAggregateFunction {
  override def inputSchema: StructType = StructType(Seq(
    StructField("offset", LongType, nullable = false),
    StructField("value", IntegerType, nullable = true)
  ))

  override def bufferSchema: StructType = StructType(Seq(
    StructField("offset", LongType, nullable = false),
    StructField("value", IntegerType, nullable = true)
  ))

  override def dataType: DataType = bufferSchema

  override def deterministic: Boolean = true

  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, -1L)
  }

  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val inputOffset = input.getLong(0)
    if (buffer.getLong(0) < inputOffset) {
      buffer.update(0, input.getLong(0))
      buffer.update(1, input.get(1))
    }
  }

  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    if (buffer1.getLong(0) < buffer2.getLong(0)) {
      buffer1.update(0, buffer2.getLong(0))
      buffer1.update(1, buffer2.get(1))
    }
  }

  override def evaluate(buffer: Row): Any = buffer
}

object SnapshotAggregator_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("SnapshotAggregator_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new SnapshotAggregator()

    val schema = StructType(Seq(
      StructField("key", IntegerType, nullable = false),
      StructField("offset", LongType, nullable = false),
      StructField("value", IntegerType, nullable = true)
    ))

    val predicate = $"offset" > 20

		val parquetPath = "SnapshotAggregator_14.parquet"
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
              "key" -> Zipf(1_000_000, 1.25),
              // "offset" -> Gaussian(10.0, 5.0),
              "offset" -> Zipf(40, 1.75),
              "value" -> Zipf(800, 1.25)),
            seed = Some(42L)
          )
          .withColumn("key", format_string("k%04d", $"key"))
          // .withColumn("offset", $"offset" - 12)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"key").agg(udaf($"offset", $"value").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      ($"offset" > 20 && $"offset" =!= 25 && $"offset" < 30) ||
      ($"offset" >= 35 && $"offset" < 98))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"key").agg(udaf($"offset", $"value").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"offset" < 98 &&
      $"offset" =!= 25 &&
      ($"offset" < 30 || $"offset" >= 35) &&
      $"offset" =!= -1)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
