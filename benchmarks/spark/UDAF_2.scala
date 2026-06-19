import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object UDAF_2 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UDAF_2")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("state", StringType, nullable = false),
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
      $"row" > 500 &&
      !($"row" >= 1000 && $"row" < 1300))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"state").agg(udaf($"sales").as("row"))
    val filtered_pre = out_pre.filter(
      $"row" =!= 0 &&
      ($"row" < 1000 || $"row" >= 1300))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
