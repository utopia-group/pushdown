import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

object KeepRowWithMaxQuality_6 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("KeepRowWithMaxQuality_6")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new KeepRowWithMaxQuality()

    val schema = StructType(Seq(
      StructField(udaf.KEY_MIN_READ_BARCODE + "_num", IntegerType),
			StructField(udaf.KEY_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_SEQUENCE, IntegerType),
			StructField(udaf.KEY_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_QUALITY_SCORE, IntegerType),
			StructField(udaf.KEY_S_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_S_SEQUENCE, IntegerType),
			StructField(udaf.KEY_S_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_S_QUALITY_SCORE, IntegerType),
      StructField(udaf.KEY_ACC_QUALITY_SCORE, LongType)))

    val predicate = col(udaf.KEY_ACC_QUALITY_SCORE) >= 90

		val parquetPath = "KeepRowWithMaxQuality_11.parquet"
    val fs          = FileSystem.get(spark.sparkContext.hadoopConfiguration)
    val pathObj     = new Path(parquetPath)
		val df =
      if (fs.exists(pathObj)) {
        // println(s"⏩  Using cached data at $parquetPath")
        spark.read.parquet(parquetPath)
      } else {
        // println(s"🔄  Generating fresh input and writing to $parquetPath
        DataSynth
          .generate(
            spark,
            schema,
            rows = 100_000_000,
						distributions = Map(
							udaf.KEY_MIN_READ_BARCODE -> Zipf(100_000, 1.15),
							udaf.KEY_SEQUENCE_IDENTIFIER -> Zipf(250, 1.05),
							udaf.KEY_QUALITY_SCORE_IDENTIFIER -> Zipf(250, 1.05),
							udaf.KEY_QUALITY_SCORE -> Zipf(100, 1.15),
							udaf.KEY_S_SEQUENCE_IDENTIFIER -> Zipf(250, 1.05),
							udaf.KEY_S_SEQUENCE -> Zipf(250, 1.05),
							udaf.KEY_S_QUALITY_SCORE_IDENTIFIER -> Zipf(250, 1.05),
							udaf.KEY_S_QUALITY_SCORE -> Zipf(100, 1.15),
							udaf.KEY_ACC_QUALITY_SCORE -> Zipf(100, 1.15)),
            seed = Some(42L))
          .withColumn(
						udaf.KEY_MIN_READ_BARCODE,
						format_string("bc_%05d", col(udaf.KEY_MIN_READ_BARCODE))
					)
          .write.mode("overwrite").parquet(parquetPath)

        spark.read.parquet(parquetPath)
      }
      .cache()
    df.count()

		val udafCols = df.columns
			.filterNot(_ == udaf.KEY_MIN_READ_BARCODE)
			.map(col) 

    val t0 = System.nanoTime()

    val out = df.groupBy(col(udaf.KEY_MIN_READ_BARCODE)).agg(udaf(udafCols: _*).as("row"))
    val exploded = out.select($"row.*")
    val filtered = exploded.filter(
      !col(udaf.KEY_SEQUENCE_IDENTIFIER).isNull && col(udaf.KEY_SEQUENCE_IDENTIFIER) >= 1 &&
      !col(udaf.KEY_S_SEQUENCE).isNull && col(udaf.KEY_S_SEQUENCE) =!= 200 &&
      col(udaf.KEY_ACC_QUALITY_SCORE) >= 90)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy(col(udaf.KEY_MIN_READ_BARCODE)).agg(udaf(udafCols: _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(
      (col(udaf.KEY_S_SEQUENCE).isNull || col(udaf.KEY_S_SEQUENCE) =!= 200) &&
      col(udaf.KEY_ACC_QUALITY_SCORE) =!= 0 &&
      (col(udaf.KEY_SEQUENCE_IDENTIFIER).isNull || col(udaf.KEY_SEQUENCE_IDENTIFIER) >= 1))

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
