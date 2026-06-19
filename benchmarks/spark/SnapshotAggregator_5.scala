import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object SnapshotAggregator_5 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("SnapshotAggregator_5")
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

    val predicate = $"offset" > 10

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
      (($"offset" > 10 && $"offset" < 34) || $"offset" >= 35) &&
      (!$"value".isNull && ($"value" < 10 || $"value" >= 21)))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9

    
    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"key").agg(udaf($"offset", $"value").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"offset" =!= -1 && ($"offset" < 34 || $"offset" >= 35) &&
      ($"value".isNull || $"value" < 10 || $"value" >= 21))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
