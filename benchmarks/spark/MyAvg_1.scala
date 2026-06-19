// https://github.com/bigdataguide/AuraCasesTraining/blob/2068cba9f3d206700aec13c826ab237633272de2/funnel-analysis/src/main/java/com/aura/funnel/spark/MyAvg.java

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import scala.util.Random

class MyAvg extends UserDefinedAggregateFunction {

  private val _inputSchema: StructType =
    StructType(Seq(StructField("inputDouble", DoubleType, nullable = true)))

  private val _bufferSchema: StructType = StructType(Seq(
    StructField("bufferSum"  , DoubleType, nullable = true),
    StructField("bufferCount", LongType  , nullable = true)
  ))

  override def inputSchema : StructType = _inputSchema
  override def bufferSchema: StructType = _bufferSchema
  override def dataType    : DataType   = _bufferSchema
  override def deterministic: Boolean   = true

  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, null)
    buffer.update(1, 0L)
  }

  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    if (!input.isNullAt(0)) {
      if (buffer.isNullAt(0)) {
        buffer.update(0, input.getDouble(0))
        buffer.update(1, 1L)
      } else {
        buffer.update(0, input.getDouble(0) + buffer.getDouble(0))
        buffer.update(1, buffer.getLong(1) + 1L)
      }
    }
  }

  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    if (!buffer2.isNullAt(0)) {
      if (buffer1.isNullAt(0)) {
        buffer1.update(0, buffer2.getDouble(0))
        buffer1.update(1, buffer2.getLong(1))
      } else {
        buffer1.update(0, buffer2.getDouble(0) + buffer1.getDouble(0))
        buffer1.update(1, buffer1.getLong(1) + buffer2.getLong(1))
      }
    }
  }

  override def evaluate(buffer: Row): Any = buffer
}

object MyAvg_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("MyAvg_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new MyAvg()

    val schema = StructType(Seq(
      StructField("gender", IntegerType, nullable = false),
      StructField("inputDouble", IntegerType, nullable = true)))

		val predicate = !$"inputDouble".isNull
  
    val gender_lbls = Seq("x", "u", "female", "male")
		
		val parquetPath = "MyAvg_12.parquet"
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
              "gender" -> Zipf(gender_lbls.size, 1.50),
              "inputDouble" -> Zipf(100_001, 1.75)),
            seed = Some(42L))
          .withColumn(
            "gender",
            udf((i: Int) => gender_lbls(i - 1)).apply($"gender")
          )
          .withColumn(
            "inputDouble",
            when($"inputDouble".between(1, 10), lit(null).cast(DoubleType))
              .otherwise($"inputDouble".cast(DoubleType))
          )
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"gender").agg(udaf($"inputDouble").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"bufferSum".isNull && $"bufferSum" >= 2_500_000 && $"bufferSum" =!= 5_000_000 &&
      $"bufferCount" > 5_000_000 && $"bufferCount" <= 25_000_000)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy("gender").agg(udaf($"inputDouble").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      ($"bufferSum".isNull || ($"bufferSum" =!= 5000000 && $"bufferSum" >= 2500000)) &&
      $"bufferCount" <= 25000000 && $"bufferCount" > 5000000)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
