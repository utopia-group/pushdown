import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import java.sql.{ Date, Timestamp }
import java.util.Calendar
import java.time.LocalDate

object SparkDataFrameUDFApp_4 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("SparkDataFrameUDFApp_4")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val current = DateRange(Timestamp.valueOf("2015-01-01 00:00:00"), Timestamp.valueOf("2015-12-31 00:00:00"))
    val udaf = new YearOnYearUDAF(current)

    val schema = StructType(Seq(
      StructField("id", IntegerType, nullable = false),
      StructField("metric", DoubleType, nullable = false),
      StructField("time", DateType, nullable = true)
    ))

    def minusOneYear(ts: Timestamp): Date =
      new Date(ts.toInstant.minusSeconds(365L * 24 * 3600).toEpochMilli)

    val curStart  = Date.valueOf("2015-01-01")
    val curEnd    = Date.valueOf("2015-12-31")
    val prevStart = minusOneYear(Timestamp.valueOf("2015-01-01 00:00:00"))
    val prevEnd   = minusOneYear(Timestamp.valueOf("2015-12-31 00:00:00"))

    val predicate =
      $"time".between(lit(curStart) , lit(curEnd)) ||
      $"time".between(lit(prevStart), lit(prevEnd))

    def d(s: String): Long = LocalDate.parse(s).toEpochDay

		val parquetPath = "SparkDataFrameUDFApp_12.parquet"
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
              "id"     -> Zipf(5_000, 1.50),
              "metric" -> Zipf(200, 1.35),
              "time"   -> Zipf(4 * 365 + 1, 1.55)),
            seed = Some(42L)
          )
          .withColumn("time", date_add(to_date(lit("2013-01-01")), datediff($"time", to_date(lit("1970-01-01"))) - 1))
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"id").agg(udaf($"metric", $"time").as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      $"sumOfCurrent" >= 5 && $"sumOfCurrent" <= 1250 &&
      $"sumOfPrevious" > 5 && $"sumOfPrevious" < 3000)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"id").agg(udaf($"metric", $"time").as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"sumOfCurrent" >= 5 && $"sumOfCurrent" <= 1250 &&
      $"sumOfPrevious" > 5 && $"sumOfPrevious" < 3000)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
