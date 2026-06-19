// https://github.com/Mageswaran1989/aja/blob/5a54a9c4c0de0bd2394b05d72b0a86916ae59770/src/examples/scala/org/aja/tej/examples/sparksql/sql/UDAF.scala#L68

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class ScalaAggregateFunction2 extends UserDefinedAggregateFunction {

  //inputSchema: inputSchema returns a StructType and every field of this StructType
  // represents an input argument of this UDAF.

  // an aggregation function can take multiple arguments in general. but
  // this one just takes one
  def inputSchema: StructType =
    new StructType().add("sales", DoubleType)

  //bufferSchema: bufferSchema returns a StructType and every field of this StructType
  // represents a field of this UDAF’s intermediate results.

  // the aggregation buffer can also have multiple values in general but
  // this one just has one: the partial sum
  def bufferSchema: StructType =
    new StructType().add("sumLargeSales", DoubleType)

  //dataType: dataType returns a DataType representing the data type of this UDAF’s returned value.

  // returns just a double: the sum
  def dataType: DataType = DoubleType

  //deterministic: deterministic returns a boolean indicating if this UDAF always generate the same
  // result for a given set of input values.

  // always gets the same result/
  def deterministic: Boolean = true


  //initialize: initialize is used to initialize values of an aggregation buffer, represented by a MutableAggregationBuffer.

  // each partial sum is initialized to zero
  def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, 0.0)
  }

  //update: update is used to update an aggregation buffer represented by a MutableAggregationBuffer for an input Row.

  // an individual sales value is incorporated by adding it if it exceeds 500.0
  def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val sum = buffer.getDouble(0)
    if (!input.isNullAt(0)) {
      val sales = input.getDouble(0)
      if (sales > 500.0) {
        buffer.update(0, sum + sales)
      }
    }
  }

  //  merge: merge is used to merge two aggregation buffers and store the result to a MutableAggregationBuffer.

  // buffers are merged by adding the single values in them
  def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    buffer1.update(0, buffer1.getDouble(0) + buffer2.getDouble(0))
  }

  //evaluate: evaluate is used to generate the final result value of this UDAF based on values
  // stored in an aggregation buffer represented by a Row.

  // the aggregation buffer just has one value: so return it
  def evaluate(buffer: Row): Any = {
    buffer.getDouble(0)
  }
}

object UDAF_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UDAF_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("state", IntegerType, nullable = false),
      StructField("sales", DoubleType, nullable = true)
    ))

    val udaf = new ScalaAggregateFunction2()

    val predicate = $"sales" > 500

    val states = Array(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
      "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
      "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

		val parquetPath = "UDAF_13.parquet"
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
              "state" -> Zipf(states.size, 1.50),
              // "sales" -> Gaussian(500.0, 200.0)
              "sales" -> Zipf(600, 1.50)
            ),
            seed = Some(42L)
          )
          .withColumn("state", udf((i: Int) => states(i - 1)).apply($"state"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"state").agg(udaf($"sales").as("row"))
    val filtered = out.filter(
      $"row" > 1000 &&
      $"row" =!= 1500)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"state").agg(udaf($"sales").as("row"))
    val filtered_pre = out_pre.filter(
      $"row" > 1000 &&
      $"row" =!= 1500)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
