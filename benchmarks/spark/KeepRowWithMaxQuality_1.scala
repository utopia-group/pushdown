// https://github.com/hamshif/Pipelines/blob/a2e32c8d3807038d860fe75eea86ee786f773251/Fastq/src/main/scala/com/hamshif/wielder/pipelines/fastq/KeepRowWithMaxQuality.scala#L8
// https://github.com/hamshif/Pipelines/blob/a2e32c8d3807038d860fe75eea86ee786f773251/Fastq/src/main/scala/com/hamshif/wielder/pipelines/fastq/FastQ.scala#L171

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class KeepRowWithMaxQuality extends UserDefinedAggregateFunction {
  val KEY_FASTQ = "fastq"

  val KEY_SEQUENCE_IDENTIFIER = "sequence_identifier"
  val KEY_SEQUENCE = "sequence"
  val KEY_QUALITY_SCORE_IDENTIFIER = "quality_score_identifier"
  val KEY_QUALITY_SCORE = "quality"

  val KEY_UNIQUE = "unique"

  val KEY_SHORT = "s_"

  val KEY_S_SEQUENCE_IDENTIFIER = s"$KEY_SHORT$KEY_SEQUENCE_IDENTIFIER"
  val KEY_S_SEQUENCE = s"$KEY_SHORT$KEY_SEQUENCE"
  val KEY_S_QUALITY_SCORE_IDENTIFIER = s"$KEY_SHORT$KEY_QUALITY_SCORE_IDENTIFIER"
  val KEY_S_QUALITY_SCORE = s"$KEY_SHORT$KEY_QUALITY_SCORE"
  val KEY_S_UNIQUE = s"$KEY_SHORT$KEY_UNIQUE"

  val KEY_UMI = "umi"
  val KEY_BARCODE = "barcode"
  val KEY_MIN_READ = "min_read"
  val KEY_MIN_READ_BARCODE = "min_read_barcode"
  val KEY_ACC_QUALITY_SCORE = s"accumulated_quality"

  val KEY_FILTERED_DUPLICATES = "filtered_duplicates"
  val KEY_FILTERED_SIMILAR = "filtered_similar"

  val SEQUENCE_DF_FIELDS = Array[String](
    KEY_SEQUENCE_IDENTIFIER,
    KEY_SEQUENCE,
    KEY_QUALITY_SCORE_IDENTIFIER,
    KEY_QUALITY_SCORE
  )

  val BARCODE_DF_FIELDS = Array[String](
    KEY_S_SEQUENCE_IDENTIFIER,
    KEY_S_SEQUENCE,
    KEY_S_QUALITY_SCORE_IDENTIFIER,
    KEY_S_QUALITY_SCORE
  )

  val DERIVED = Array[String](
    KEY_UNIQUE,
    KEY_UMI,
    KEY_BARCODE,
    KEY_MIN_READ,
    KEY_ACC_QUALITY_SCORE
  )

  val COMBINED = BARCODE_DF_FIELDS ++ SEQUENCE_DF_FIELDS ++ DERIVED

  val KEY_ROW_WITH_MAX_QUALITY = "row_with_max_quality"

	// This is the input fields for your aggregate function.
	override def inputSchema: org.apache.spark.sql.types.StructType =
		StructType(
			StructField(KEY_SEQUENCE_IDENTIFIER, IntegerType) ::
			StructField(KEY_SEQUENCE, IntegerType) ::
			StructField(KEY_QUALITY_SCORE_IDENTIFIER, IntegerType) ::
			StructField(KEY_QUALITY_SCORE, IntegerType) ::

			StructField(KEY_S_SEQUENCE_IDENTIFIER, IntegerType) ::
			StructField(KEY_S_SEQUENCE, IntegerType) ::
			StructField(KEY_S_QUALITY_SCORE_IDENTIFIER, IntegerType) ::
			StructField(KEY_S_QUALITY_SCORE, IntegerType) ::

			StructField(KEY_ACC_QUALITY_SCORE, LongType) :: Nil
		)

	// This is the internal fields you keep for computing your aggregate.
	override def bufferSchema: StructType =
		StructType(
			StructField(KEY_SEQUENCE_IDENTIFIER, IntegerType) ::
			StructField(KEY_SEQUENCE, IntegerType) ::
			StructField(KEY_QUALITY_SCORE_IDENTIFIER, IntegerType) ::
			StructField(KEY_QUALITY_SCORE, IntegerType) ::

			StructField(KEY_S_SEQUENCE_IDENTIFIER, IntegerType) ::
			StructField(KEY_S_SEQUENCE, IntegerType) ::
			StructField(KEY_S_QUALITY_SCORE_IDENTIFIER, IntegerType) ::
			StructField(KEY_S_QUALITY_SCORE, IntegerType) ::

			StructField(KEY_ACC_QUALITY_SCORE, LongType) :: Nil
		)


	// This is the output type of your aggregation function.
	override def dataType: DataType =
		StructType((Array(
			StructField(KEY_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(KEY_SEQUENCE, IntegerType),
			StructField(KEY_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(KEY_QUALITY_SCORE, IntegerType),

			StructField(KEY_S_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(KEY_S_SEQUENCE, IntegerType),
			StructField(KEY_S_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(KEY_S_QUALITY_SCORE, IntegerType),

			StructField(KEY_ACC_QUALITY_SCORE, LongType)
		)))

	override def deterministic: Boolean = true

	// This is the initial value for your buffer schema.
	override def initialize(buffer: MutableAggregationBuffer): Unit = {
		buffer(0) = null
		buffer(1) = null
		buffer(2) = null
		buffer(3) = null
		buffer(4) = null
		buffer(5) = null
		buffer(6) = null
		buffer(7) = null
		buffer(8) = 0L
	}

	// This is how to update your buffer schema given an input.
	override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {

		val amt = buffer.getAs[Long](8)
		val candidateAmt = input.getAs[Long](8)

		if(amt < candidateAmt) {

				buffer(0) = input.getAs[Integer](0)
				buffer(1) = input.getAs[Integer](1)
				buffer(2) = input.getAs[Integer](2)
				buffer(3) = input.getAs[Integer](3)
				buffer(4) = input.getAs[Integer](4)
				buffer(5) = input.getAs[Integer](5)
				buffer(6) = input.getAs[Integer](6)
				buffer(7) = input.getAs[Integer](7)
				buffer(8) = input.getAs[Long](8)

		}
	}

	// This is how to merge two objects with the bufferSchema type.
	override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {

		buffer1(0) = buffer2.getAs[Integer](0)
		buffer1(1) = buffer2.getAs[Integer](1)
		buffer1(2) = buffer2.getAs[Integer](2)
		buffer1(3) = buffer2.getAs[Integer](3)
		buffer1(4) = buffer2.getAs[Integer](4)
		buffer1(5) = buffer2.getAs[Integer](5)
		buffer1(6) = buffer2.getAs[Integer](6)
		buffer1(7) = buffer2.getAs[Integer](7)
		buffer1(8) = buffer2.getAs[Long](8)
	}

	// This is where you output the final value, given the final value of your bufferSchema.
	override def evaluate(buffer: Row): Any = {
		buffer
	}
}

object KeepRowWithMaxQuality_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("KeepRowWithMaxQuality_1")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    import DistSpec._

    val udaf = new KeepRowWithMaxQuality()

    val schema = StructType(Seq(
      StructField(udaf.KEY_MIN_READ_BARCODE, IntegerType),
			StructField(udaf.KEY_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_SEQUENCE, IntegerType),
			StructField(udaf.KEY_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_QUALITY_SCORE, IntegerType),
			StructField(udaf.KEY_S_SEQUENCE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_S_SEQUENCE, IntegerType),
			StructField(udaf.KEY_S_QUALITY_SCORE_IDENTIFIER, IntegerType),
			StructField(udaf.KEY_S_QUALITY_SCORE, IntegerType),
      StructField(udaf.KEY_ACC_QUALITY_SCORE, LongType)))

    val predicate = col(udaf.KEY_ACC_QUALITY_SCORE) < 0

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
    val filtered = exploded.filter(col(udaf.KEY_ACC_QUALITY_SCORE) < 0)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy(col(udaf.KEY_MIN_READ_BARCODE)).agg(udaf(udafCols: _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter(col(udaf.KEY_ACC_QUALITY_SCORE) =!= 0)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
