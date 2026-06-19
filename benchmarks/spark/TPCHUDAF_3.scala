import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object TPCHUDAF_3 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("TPCHUDAF_3")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val schema = StructType(Seq(
      StructField("l_shipmode", StringType, nullable = false),
      StructField("o_orderpriority", StringType, nullable = false)
    ))

    val udaf = new UDAF_Q12_HIGH()

    val predicate = $"o_orderpriority" === "1-URGENT" || $"o_orderpriority" === "2-HIGH"

    val shipModes = Array("MAIL","RAIL","TRUCK","SHIP","AIR")

		val parquetPath = "TPCHUDAF_12.parquet"
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
            rows = 100_000,
            distributions = Map(
              "l_shipmode" -> CategoricalString.uniform(shipModes: _*),
              "o_orderpriority" -> CategoricalString.uniform(
                "1-URGENT", "2-HIGH", "3-MEDIUM", "4-LOW"
              )
            ),
            seed = Some(42L)
          )
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"l_shipmode").agg(udaf($"o_orderpriority").as("row"))
    val filtered = out.filter(!($"row" < 0))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9

    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"l_shipmode").agg(udaf($"o_orderpriority").as("row"))

    val result_pre = out_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${runtime_pre}%.2f")

    spark.stop()
  }
}
