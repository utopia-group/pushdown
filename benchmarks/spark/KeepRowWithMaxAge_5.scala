import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object KeepRowWithMaxAge_5 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("KeepRowWithMaxAge_5")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new KeepRowWithMaxAge()

    val schema = StructType(Seq(
			StructField("id", LongType),
			StructField("name", StringType),
			StructField("requisite", StringType),
			StructField("money", DoubleType),
			StructField("age", IntegerType)))

    val predicate = $"age" > 10

		val parquetPath = "KeepRowWithMaxAge_11.parquet"
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
							"id" -> Sequential(1, 100_000_000, 1),
							"name" -> CategoricalString.uniform("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Heidi", "Ivan", "Judy", "Mallory", "Niaj", "Olivia", "Peggy", "Quentin", "Rupert", "Sybil", "Trent", "Uma", "Victor", "Wendy", "Xander", "Yvonne", "Zack"),
							"requisite" -> CategoricalString.uniform("Laugh", "Twist", "Smile", "Frown"),
							"money" -> Zipf(100_000, 1.15),
							"age" -> Zipf(120, 1.40)),
            seed = Some(42L))
					.withColumn("age", $"age" - 1)
          .write.mode("overwrite").parquet(parquetPath)

        // read it back to ensure identical code path
        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

    val t0 = System.nanoTime()

    val out = df.groupBy($"money").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      $"id" >= 24 &&
      !($"requisite" === "Laugh") &&
      $"age" > 10)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"money").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      $"requisite" =!= "Laugh" &&
      $"id" >= 24)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
