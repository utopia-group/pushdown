import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object AggregationQuerySuite_2 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("AggregationQuerySuite_2")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("id", IntegerType, nullable = false),
      StructField("key", IntegerType, nullable = true),
      StructField("value1", FloatType, nullable = true),
      StructField("value2", FloatType, nullable = true)))

    val udaf = new ScalaAggregateFunction1(StructType(schema.drop(1)))

    val predicate = $"key" === 50

		val parquetPath = "AggregationQuerySuite_11.parquet"
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
              "id" -> Zipf(1_000, 1.5),
              "key" -> Zipf(50, 1.5),
              "value1" -> Zipf(60, 1.5),
              "value2" -> Zipf(60, 1.5)),
            seed = Some(42L))
          .withColumn(
            "key",
            when(rand() < 0.15, lit(null).cast(IntegerType))
              .otherwise($"key")
          )
          .withColumn("value1", $"value1" + 40)
          .withColumn("value2", $"value2" + 40)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"id").agg(udaf($"key", $"value1", $"value2").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !$"key".isNull && $"key" =!= 60 &&
      !$"value1".isNull && $"value1" > 22 &&
      !$"value2".isNull && $"value2" <= 44)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"id").agg(udaf($"key", $"value1", $"value2").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"key".isNotNull &&
      $"value1" > 22 &&
      $"value2" <= 44)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
