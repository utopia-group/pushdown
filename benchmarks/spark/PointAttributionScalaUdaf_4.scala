import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}
import java.util.ArrayList

object PointAttributionScalaUdaf_4 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("PointAttributionScalaUdaf_4")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new PointAttributionScalaUdaf()

    val schema = StructType(Seq(
      StructField("firstName", IntegerType, nullable = false),
      StructField("lastName", IntegerType, nullable = false),
      StructField("state", IntegerType, nullable = false),
      StructField("_c0", IntegerType, nullable = true)))

		val predicate = $"_c0".isNotNull

    val firstNames = Seq(
      "James","Mary","John","Patricia","Robert","Jennifer",
      "Michael","Linda","William","Elizabeth","David","Barbara")

    val lastNames = Seq(
      "Smith","Johnson","Williams","Brown","Jones","Garcia",
      "Miller","Davis","Rodriguez","Martinez","Hernandez","Lopez")

    val states = Seq("CA","TX","FL","NY","IL","PA","OH","GA","NC","MI","NJ","VA")

    // convenience:  label i ↔ value pools(i-1)
    def labelTo(col: Column, values: Seq[String]): Column =
      values.zipWithIndex.foldLeft(lit(values.last): Column) {
        case (acc, (v, idx)) => when(col === idx + 1, lit(v)).otherwise(acc)
      }
		
		val parquetPath = "PointAttributionScalaUdaf_11.parquet"
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
              "firstName" -> Zipf(firstNames.size, 1.15),
              "lastName" -> Zipf(lastNames.size, 1.15),
              "state" -> Zipf(states.size, 1.15),
              "_c0" -> Zipf(6, 1.70)
            ),
            seed = Some(42L))
          .withColumn("firstName", labelTo($"firstName", firstNames))
          .withColumn("lastName", labelTo($"lastName", lastNames))
          .withColumn("state", labelTo($"state", states))
          .withColumn(
            "_c0",
            when(rand() < 0.9, lit(null).cast(IntegerType))
              .otherwise($"_c0" - 1)
          )
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"firstName", $"lastName", $"state").agg(udaf($"_c0").as("row"))
    val filtered = out.filter(
      ($"row" <= -10 && $"row" =!= -77) ||
      ($"row" =!= 100 && $"row" > 50))

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"firstName", $"lastName", $"state").agg(udaf($"_c0").as("row"))
    val filtered_pre = out_pre.filter(
      ($"row" <= -10 && $"row" =!= -77) ||
      ($"row" =!= 100 && $"row" > 50))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
