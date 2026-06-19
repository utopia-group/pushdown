// https://github.com/ExposuresProvider/FHIR-PIT/blob/fd9bd72fb062f9162286724abe7c7b03b6995842/spark/src/main/scala/datatrans/step/PreprocCSVTable.scala#L146

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class TotalTypeVisits extends UserDefinedAggregateFunction {
  val vtype = Seq("IMP")
  val nvtype = Seq("AMB","EMER")

  // This is the input fields for your aggregate function.
  override def inputSchema: org.apache.spark.sql.types.StructType =
    StructType(StructField("VisitType", StringType) :: StructField("RespiratoryDx", BooleanType) :: Nil)

  // This is the internal fields you keep for computing your aggregate.
  override def bufferSchema: StructType = StructType(
    StructField("count", IntegerType) :: Nil
  )

  // This is the output type of your aggregatation function.
  override def dataType: DataType = IntegerType

  override def deterministic: Boolean = true

  // This is the initial value for your buffer schema.
  override def initialize(buffer: MutableAggregationBuffer): Unit = {
    buffer(0) = 0
  }

  // This is how to update your buffer schema given an input.
  override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {
    val visitType = input.getAs[String](0)
    val respiratoryDx = input.getAs[Boolean](1)
    if(respiratoryDx && visitType != null && vtype.exists(visitType.contains(_)) && !nvtype.exists(visitType.contains(_))) {
      buffer(0) = buffer.getAs[Int](0) + 1
    }
  }

  // This is how to merge two objects with the bufferSchema type.
  override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {
    buffer1(0) = buffer1.getAs[Int](0) + buffer2.getAs[Int](0)
  }

  // This is where you output the final value, given the final value of your bufferSchema.
  override def evaluate(buffer: Row): Any = {
    buffer.getInt(0)
  }
}

object PreprocCSVTable_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("PreprocCSVTable_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new TotalTypeVisits()

    val schema = StructType(Seq(
      StructField("patient", IntegerType, nullable = false),
      StructField("VisitType", IntegerType, nullable = true),
      StructField("RespiratoryDx", IntegerType, nullable = false)))

		/* helper that OR-reduces a Seq of substring tests */
    def anyContains(col: Column, needles: Seq[String]): Column =
      needles.map(col.contains).reduce(_ || _)

    val predicate =
      $"VisitType".isNotNull && $"RespiratoryDx" &&
      anyContains($"VisitType", udaf.vtype) &&
      !anyContains($"VisitType", udaf.nvtype)

		val parquetPath = "PreprocCSVTable_11.parquet"
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
              "patient" -> Sequential(1, 5_000, 1),
              "VisitTypeCode" -> Zipf(4, 1.05),
              "RespDxCode" -> Zipf(2, 2.0)),
            seed = Some(42L))
          .withColumn(
            "VisitType",
            when($"VisitType" === 1, "AMB")
              .when($"VisitType" === 2, "IMP")
              .when($"VisitType" === 3, "EMER")
              .otherwise("OBS"))
          .withColumn("RespiratoryDx", $"RespiratoryDx" === 2)
          .withColumn("patient", format_string("pa%04d", $"patient"))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"patient").agg(udaf($"Visittype", $"RespiratoryDx").as("row"))
    val filtered = out.filter($"row" > 10 && !($"row" >= 29 && $"row" < 63) && $"row" < 100)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"patient").agg(udaf($"Visittype", $"RespiratoryDx").as("row"))
    val filtered_pre = out_pre.filter($"row" > 10 && !($"row" >= 29 && $"row" < 63) && $"row" < 100)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
