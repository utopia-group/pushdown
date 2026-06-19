// https://github.com/jgperrin/net.jgp.books.spark.ch15/blob/38e2becbf643f670e63a7a87deceb98072b58052/src/main/scala/net/jgp/books/spark/ch15/lab400_udaf/PointAttributionScalaUdaf.scala
// https://github.com/jgperrin/net.jgp.books.spark.ch15/blob/38e2becbf643f670e63a7a87deceb98072b58052/src/main/scala/net/jgp/books/spark/ch15/lab400_udaf/PointsPerOrderScalaApp.scala#L27

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import java.util.ArrayList

class PointAttributionScalaUdaf extends UserDefinedAggregateFunction {

  val MAX_POINT_PER_ORDER = 3

  /**
   * Describes the schema of input sent to the UDAF. Spark UDAFs can operate
   * on any number of columns. In our use case, we only need one field.
   */
  override def inputSchema: StructType = {
    val inputFields = new ArrayList[StructField]
    inputFields.add(DataTypes.createStructField("_c0", DataTypes.IntegerType, true))
    DataTypes.createStructType(inputFields)
  }

  /**
   * Describes the schema of UDAF buffer.
   */
  override def bufferSchema: StructType = {
    val bufferFields = new ArrayList[StructField]
    bufferFields.add(DataTypes.createStructField("sum", DataTypes.IntegerType, true))
    DataTypes.createStructType(bufferFields)
  }

  /**
   * Datatype of the UDAF's output.
   */
  override def dataType: DataType = DataTypes.IntegerType

  /**
   * Describes whether the UDAF is deterministic or not.
   *
   * As Spark executes by splitting data, it processes the chunks separately
   * and combining them. If the UDAF logic is such that the result is
   * independent of the order in which data is processed and combined then
   * the UDAF is deterministic.
   */
  override def deterministic = true

  /**
   * Initializes the buffer. This method can be called any number of times
   * of Spark during processing.
   *
   * The contract should be that applying the merge function on two initial
   * buffers should just return the initial buffer itself, i.e.
   * `merge(initialBuffer, initialBuffer)` should equal `initialBuffer`.
   */
  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, // column
      0) // value

    // You can repeat that for the number of columns you have in your
    // buffer
  }

  /**
   * Updates the buffer with an input row. Validations on input should be
   * performed in this method.
   */
  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    if (input.isNullAt(0)) {
      return
    }
    // Apply your business rule, could be in an external function/service.
    val initialValue = buffer.getInt(0)
    val inputValue = input.getInt(0)
    var outputValue = 0
    if (inputValue < MAX_POINT_PER_ORDER) outputValue = inputValue
    else outputValue = MAX_POINT_PER_ORDER
    outputValue += initialValue
    buffer.update(0, outputValue)
  }

  /**
   * Merges two aggregation buffers and stores the updated buffer values
   * back to buffer.
   */
  override def merge(buffer: MutableAggregationBuffer, row: Row): Unit = {
    buffer.update(0, buffer.getInt(0) + row.getInt(0))
  }

  /**
   * Calculates the final result of this UDAF based on the given aggregation
   * buffer.
   */
  override def evaluate(row: Row): Integer = {
    row.getInt(0)
  }

}

object PointAttributionScalaUdaf_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("PointAttributionScalaUdaf_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new PointAttributionScalaUdaf()

    val schema = StructType(Seq(
      StructField("firstName", IntegerType, nullable = false),
      StructField("lastName", IntegerType, nullable = false),
      StructField("state", IntegerType, nullable = false),
      StructField("_c0", IntegerType, nullable = true)))

		val predicate = $"_c0".isNotNull

    val firstNames = Seq(
      "James","Mary","John","Patricia","Robert","Jennifer",
      "Michael","Linda","William","Elizabeth","David","Barbara")

    val lastNames = Seq(
      "Smith","Johnson","Williams","Brown","Jones","Garcia",
      "Miller","Davis","Rodriguez","Martinez","Hernandez","Lopez")

    val states = Seq(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
      "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
      "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
    )

    // convenience:  label i ↔ value pools(i-1)
    def labelTo(col: Column, values: Seq[String]): Column =
      values.zipWithIndex.foldLeft(lit(values.last): Column) {
        case (acc, (v, idx)) => when(col === idx + 1, lit(v)).otherwise(acc)
      }
		
		val parquetPath = "PointAttributionScalaUdaf_11.parquet"
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
              "firstName" -> Zipf(firstNames.size, 1.15),
              "lastName" -> Zipf(lastNames.size, 1.15),
              "state" -> Zipf(states.size, 1.15),
              "_c0" -> Zipf(6, 1.70)
            ),
            seed = Some(42L))
          .withColumn("firstName", labelTo($"firstName", firstNames))
          .withColumn("lastName", labelTo($"lastName", lastNames))
          .withColumn("state", labelTo($"state", states))
          .withColumn(
            "_c0",
            when(rand() < 0.9, lit(null).cast(IntegerType))
              .otherwise($"_c0" - 1)
          )
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"firstName", $"lastName", $"state").agg(udaf($"_c0").as("row"))
    val filtered = out.filter($"row" > 0 && $"row" <= 100)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"firstName", $"lastName", $"state").agg(udaf($"_c0").as("row"))
    val filtered_pre = out_pre.filter($"row" > 0 && $"row" <= 100)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
