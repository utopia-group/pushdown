// https://github.com/feathr-ai/feathr/blob/45e44afc1ebd3abc0fa8313aac21db7b1f05580a/feathr-impl/src/main/scala/com/linkedin/feathr/offline/generation/aggregations/MinPooling.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import scala.util.Random

class MinPoolingUDAF extends UserDefinedAggregateFunction {

  var embeddingSize = 3

  /**
   * Input schema of this UDAF.
   * This UDAF accepts 1 column of embedding vectors as input.
   */
  override def inputSchema: org.apache.spark.sql.types.StructType =
    StructType(Seq(StructField("value", ArrayType(DoubleType, false))))

  /**
   * Schema for the buffer Row of this UDAF.
   * This UDAF keeps track of the element-wise max of all embedding vector.
   */
  override def bufferSchema: StructType = StructType(Seq(StructField("agg", ArrayType(DoubleType, false))))

  /**
   * Output data type.
   * This UDAF outputs 1 aggregated embedding vector, as an array of double.
   */
  override def dataType: DataType = ArrayType(DoubleType, false)

  override def deterministic: Boolean = true

  /**
   * Initialize the buffer.
   */
  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = Seq.fill(embeddingSize)(Double.MaxValue)
  }

  /**
   * Update the buffer with 1 input row.
   */
  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    calculate(buffer, input)
  }

  /**
   * Merge 2 aggregation buffers.
   * For this UDAF, we take the min of 2 aggregated vectors.
   */
  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    calculate(buffer1, buffer2)
  }

  /**
   * Output the final aggregated value from the buffers.
   */
  override def evaluate(buffer: Row): Any = {
    buffer.getAs[Seq[Double]](0)
  }

  /**
   * Given the current state of the buffer and a new input row,
   * update the buffer with the value in the input row.
   *
   * For this UDAF, we calculate the element-wise min of the buffer and new input.
   */
  private def calculate(buffer: MutableAggregationBuffer, row: Row): Unit = {
    val embedding = row.getAs[Seq[Double]](0)
    val aggregate = buffer.getAs[Seq[Number]](0).map(x => x.doubleValue())
    if (embedding != null) {
      if (embedding.size != embeddingSize) {
        throw new Exception(s"embedding vector size has a length of ${embedding.size}, different from expected size ${embeddingSize}")
      }
      val newAgg = aggregate.zip(embedding).map { case (x, y) => Math.min(x, y) }
      buffer.update(0, newAgg)
    }
  }
}

object MinPooling_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("MinPooling_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new MinPoolingUDAF()

    val schema = StructType(Seq(
      StructField("anchor", IntegerType, nullable = false),
      StructField("value", ArrayType(DoubleType, false), nullable = true)))

		val predicate =
      $"value".getItem(0) < 10 ||
      $"value".getItem(1) < 10 ||
      $"value".getItem(2) < 10

    val valueCol: Column =
      when(rand() < 0.40, lit(null).cast(ArrayType(DoubleType, containsNull = false)))
        .otherwise(
          array(
            (0 until udaf.embeddingSize).map { _ =>
              (rand() * 10.0 + 6.0).cast("int")  // Values from 6 to 15
            }: _*
          )
        )

		val parquetPath = "MinPooling_11.parquet"
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
              "anchor" -> Zipf(450_000, 1.01)),  // 450k groups for ~50% selectivity
            overrides = Map(
              "value" -> valueCol),
            seed = Some(42L))
          .withColumn("anchor", format_string("id_%04d", $"anchor"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"anchor").agg(udaf($"value").as("row"))
    val filtered = out.filter(
      !$"row".getItem(0).isNull && $"row".getItem(0) < 10 &&
      !$"row".getItem(1).isNull && $"row".getItem(1) < 10 &&
      !$"row".getItem(2).isNull && $"row".getItem(2) < 10)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy("anchor").agg(udaf($"value").as("row"))
    val filtered_pre = out_pre.filter(
      $"row".getItem(0).isNotNull && $"row".getItem(0) < 10 &&
      $"row".getItem(1).isNotNull && $"row".getItem(1) < 10 &&
      $"row".getItem(2).isNotNull && $"row".getItem(2) < 10)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
