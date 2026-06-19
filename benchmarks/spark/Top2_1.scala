// https://github.com/odnoklassniki/pravda-ml/blob/7105bc0c8670fed6ce1a72fc04e4646eaa50f4c0/src/main/scala/org/apache/spark/ml/odkl/TopKUDAF.scala#L27
// https://github.com/odnoklassniki/pravda-ml/blob/7105bc0c8670fed6ce1a72fc04e4646eaa50f4c0/src/main/scala/org/apache/spark/ml/odkl/TopKTransformer.scala#L29
// https://github.com/odnoklassniki/pravda-ml/blob/7105bc0c8670fed6ce1a72fc04e4646eaa50f4c0/src/test/scala/org/apache/spark/ml/odkl/TopKTransformerSpec.scala#L13

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
// import org.apache.spark.sql.catalyst.expressions.GenericRowWithSchema
import org.apache.hadoop.fs.{FileSystem, Path}
import java.util.Comparator
// import scala.collection.mutable

class TopKUDAF[B](val numRows: Int = 20,
                  dfSchema: StructType,
                  columnToSortBy: String)
                 (implicit val cmp: Ordering[B]) extends UserDefinedAggregateFunction {

  @transient lazy val rowComparator = new Comparator[Row] {
    override def compare(o1: Row, o2: Row): Int = -cmp.compare(o1.getAs[B](columnToSortByIndex), o2.getAs[B](columnToSortByIndex))
  }
  val columnToSortByIndex: Int = dfSchema.fieldIndex(columnToSortBy)

  override def bufferSchema: StructType = new StructType().add("arrData", ArrayType(dfSchema))

  override def dataType: DataType = new StructType().add("arrData", ArrayType(dfSchema))

  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {

    var data = buffer.getSeq[Row](0).toArray

    if (data.length < numRows) {
      val indUno = java.util.Arrays.binarySearch[Row](data.asInstanceOf[Array[Row]], input, rowComparator)

      val ind: (Int, Int) = if (indUno < 0) (-indUno - 1, -indUno) else (indUno, indUno + 1)
      var dataWithEl = new Array[Row](data.length + 1)

      System.arraycopy(data, 0, dataWithEl, 0, ind._1)
      dataWithEl(ind._1) = input

      System.arraycopy(data, ind._1, dataWithEl, ind._1 + 1, data.length - ind._1)

      data = dataWithEl
    } else {

      val currentLikes = input.getAs[B](columnToSortByIndex)
      if (cmp.lt(data.last.getAs[B](columnToSortByIndex), currentLikes)) {
        val indUno = java.util.Arrays.binarySearch[Row](data.asInstanceOf[Array[Row]], input, rowComparator)

        val ind = if (indUno < 0) (-indUno - 1, -indUno) else (indUno, indUno + 1)

        var dataWithEl = new Array[Row](data.length)
        System.arraycopy(data, ind._1, data, ind._1 + 1, data.length - ind._1 - 1)
        data(ind._1) = input

      }
    }

    buffer.update(0, data.toSeq)
  }

  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    var arr1 = buffer1.getSeq[Row](0).toArray
    var arr2 = buffer2.getSeq[Row](0).toArray
    var i1 = 0
    var i2 = 0
    val ansLength = Math.min(arr1.length + arr2.length, k)
    var ans = new Array[Row](ansLength)
    var i = 0
    while (i < ansLength) {
      if (i2 >= arr2.length || i1 >= arr1.length) {
        val (ind: Int, arr: Array[Row]) = if (i2 >= arr2.length) (i1, arr1) else (i2, arr2)
        System.arraycopy(arr, ind, ans, i, Math.min(arr.length - ind, ans.length - i))
        i = ansLength
      } else if (cmp.lt(arr1(i1).asInstanceOf[Row].getAs[B](columnToSortByIndex), arr2(i2).asInstanceOf[Row].getAs[B](columnToSortByIndex))) {
        ans(i) = arr2(i2).asInstanceOf[Row]
        i2 = i2 + 1
      } else {
        ans(i) = arr1(i1).asInstanceOf[Row]
        i1 = i1 + 1
      }
      i = i + 1
    }
    buffer1.update(
      0, ans.toSeq
    )
  }

  def k = numRows

  override def inputSchema: StructType = dfSchema

  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = Seq.empty[Row]
  }

  override def deterministic: Boolean = true

  override def evaluate(buffer: Row): Any = buffer
}

object Top2_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("Top2_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("op1", IntegerType, nullable = false),
      StructField("value", LongType, nullable = false)
    ))

    val udaf_schema = StructType(Seq(
      StructField("op1", StringType, nullable = false),
      StructField("value", LongType, nullable = false)
    ))

    val udaf = new TopKUDAF[Long](2, udaf_schema, "value")

    val predicate = $"value" >= 90

    val op1Values = Array("ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT", "GOLF", "HOTEL", "INDIA", "JULIET")

		val parquetPath = "Top2_12.parquet"
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
              "op1" -> Zipf(op1Values.size, 1.50),
              // "value" -> Gaussian(90.0, 10.0)
              "value" -> Zipf(100, 1.20)
            ),
            seed = Some(42L)
          )
          .withColumn("op1", udf((i: Int) => op1Values(i - 1)).apply($"op1"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"op1").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"arrData".getItem(0).isNull &&
      $"arrData".getItem(0).getField("value") >= 90 &&
      !$"arrData".getItem(1).isNull &&
      $"arrData".getItem(1).getField("value") >= 90)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"op1").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter($"arrData".getItem(1).isNotNull)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
