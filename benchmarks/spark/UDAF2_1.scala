// https://github.com/spirom/LearningSpark/blob/8498a592e2bf6bbce8fc29dc6d7a0850a38a72f5/src/main/scala/sql/UDAF2.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

private class ScalaAggregateFunction3 extends UserDefinedAggregateFunction {

  // this aggregation function has two parameters
  def inputSchema: StructType =
    new StructType().add("sales", DoubleType).add("state", StringType)
  // the aggregation buffer can also have multiple values in general but
  // this one just has one: the partial sum
  def bufferSchema: StructType =
    new StructType().add("sumLargeSales", DoubleType)
  // returns just a double: the sum
  def dataType: DataType = DoubleType
  // always gets the same result
  def deterministic: Boolean = true

  // each partial sum is initialized to zero
  def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer.update(0, 0.0)
  }

  // these states will get special treatment
  val westernStates = Set("WA", "OR", "CA")

  // an individual sales value is incorporated by adding it if it exceeds
  // a threshold that now depends on the state
  def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val sum = buffer.getDouble(0)
    if (!input.isNullAt(0)) {
      val westernState =
        if (input.isNullAt(1)) {
          false
        } else {
          val state = input.getString(1)
          westernStates.contains(state)
        }
      val sales = input.getDouble(0)
      if ((westernState && sales > 1000.0) || (!westernState && sales > 400.0)) {
        buffer.update(0, sum+sales)
      }
    }
  }

  // buffers are merged by adding the single values in them
  def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    buffer1.update(0, buffer1.getDouble(0) + buffer2.getDouble(0))
  }

  // the aggregation buffer just has one value: so return it
  def evaluate(buffer: Row): Any = {
    buffer.getDouble(0)
  }
}

object UDAF2_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UDAF2_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("sales", DoubleType, nullable = true),
      StructField("state", IntegerType, nullable = true)
    ))

    val udaf = new ScalaAggregateFunction3()

    // val isWest = $"state".isin(udaf.westernStates.toSeq: _*)

    val predicate = $"sales" > 400.0

    val states = Array(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
      "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
      "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
    )

		val parquetPath = "UDAF2_12.parquet"
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
              // "sales" -> Gaussian(600.0, 400.0),
              "sales" -> Zipf(1_100, 1.50),
              "state" -> Zipf(states.size, 1.50),
            ),
            seed = Some(42L)
          )
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"state").agg(udaf(df.columns.map(col): _*).as("row"))
    val filtered = out.filter(
      $"row" > 401 &&
      $"row" =!= 500 &&
      $"row" < 50_000_000_000L)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"state").agg(udaf(df.columns.map(col): _*).as("row"))
    val filtered_pre = out_pre.filter(
      $"row" > 401 &&
      $"row" =!= 500 &&
      $"row" < 50_000_000_000L)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
