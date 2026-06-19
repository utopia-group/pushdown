import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object UDAF2_4 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("UDAF2_4")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("sales", DoubleType, nullable = true),
      StructField("state", StringType, nullable = true)
    ))

    val udaf = new ScalaAggregateFunction3()

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
      ($"row" <= 1_000 || $"row" >= 1_400) &&
      $"row" > 400 && $"row" =!= 500)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"state").agg(udaf(df.columns.map(col): _*).as("row"))
    val filtered_pre = out_pre.filter(
      $"row" =!= 500 && $"row" =!= 0 && 
      ($"row" <= 1_000 || $"row" === 1_400))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
