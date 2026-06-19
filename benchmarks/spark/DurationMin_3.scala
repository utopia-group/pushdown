import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.spark.unsafe.types.CalendarInterval

object DurationMin_3 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("DurationMin_3")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("months", IntegerType, nullable = false),
      StructField("days", IntegerType, nullable = false),
      StructField("micros", LongType, nullable = false)))

    val udaf = new DurationMin()

    val monthsUDF = udf((ci: CalendarInterval) => ci.months)
    val daysUDF = udf((ci: CalendarInterval) => ci.days)
    val microsUDF = udf((ci: CalendarInterval) => ci.microseconds)

    val predicate = daysUDF($"duration") < 60

    val toCal = udf((m: Int, d: Int, mi: Int) => new CalendarInterval(m, d, mi))

		val parquetPath = "DurationMin_11.parquet"
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
              "key" -> Zipf(1_000_000, 1.15),
              "months" -> Zipf(50, 1.40),
              "days" -> Zipf(30, 1.40),
              "micros" -> Zipf(100, 1.40)),
            seed = Some(42L))
          .withColumn("months", $"months" + 10)
          .withColumn("days", $"days" + 40)
          .withColumn("micros", $"micros" + 60)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
    df = df
      .withColumn("duration", toCal($"months", $"days", $"micros"))
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"key").agg(udaf($"duration").as("row"))
    val filtered = out.filter(
      !$"row".isNull &&
      monthsUDF($"row") <= 50 &&
      daysUDF($"row") < 60 &&
      (microsUDF($"row") < 76 || microsUDF($"row") === 100))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"key").agg(udaf($"duration").as("row"))
    val filtered_pre = out_pre.filter(
      $"row".isNotNull &&
      (microsUDF($"row") < 76 || microsUDF($"row") === 100) &&
      monthsUDF($"row") <= 50)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
