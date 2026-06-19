// https://github.com/hamshif/Pipelines/blob/a2e32c8d3807038d860fe75eea86ee786f773251/Snippets/src/main/scala/com/hamshif/wielder/pipelines/snippets/KeepRowWithMaxAge.scala

import org.apache.spark.sql._
import org.apache.spark.sql.expressions._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.hadoop.fs.{FileSystem, Path}

class KeepRowWithMaxAge extends UserDefinedAggregateFunction {
	// This is the input fields for your aggregate function.
	override def inputSchema: org.apache.spark.sql.types.StructType =
		StructType(
			StructField("id", LongType) ::
			StructField("name", StringType) ::
			StructField("requisite", StringType) ::
			StructField("money", DoubleType) ::
			StructField("age", IntegerType) :: Nil
		)

	// This is the internal fields you keep for computing your aggregate.
	override def bufferSchema: StructType = StructType(
		StructField("id", LongType) ::
		StructField("name", StringType) ::
		StructField("requisite", StringType) ::
		StructField("money", DoubleType) ::
		StructField("age", IntegerType) :: Nil
	)


	// This is the output type of your aggregation function.
	override def dataType: DataType =
		StructType((Array(
			StructField("id", LongType),
			StructField("name", StringType),
			StructField("requisite", StringType),
			StructField("money", DoubleType),
			StructField("age", IntegerType)
		)))

	override def deterministic: Boolean = true

	// This is the initial value for your buffer schema.
	override def initialize(buffer: MutableAggregationBuffer): Unit = {
		buffer(0) = 0L
		buffer(1) = "empty"
		buffer(2) = "empty"
		buffer(3) = 0.0
		buffer(4) = 0
	}

	// This is how to update your buffer schema given an input.
	override def update(buffer: MutableAggregationBuffer, input: Row): Unit = {

		val age = buffer.getAs[Int](4)
		val candidateAge = input.getAs[Int](4)

		// // println(s"class ${age.getClass}  age $age")
		// // println(s"class ${candidateAge.getClass}  candidateAge $candidateAge")

		age match {
			case a if a < candidateAge =>
				buffer(0) = input.getAs[Long](0)
				buffer(1) = input.getAs[String](1)
				buffer(2) = input.getAs[String](2)
				buffer(3) = input.getAs[Double](3)
				buffer(4) = input.getAs[Int](4)
			case _ =>
		}

	}

	// This is how to merge two objects with the bufferSchema type.
	override def merge(buffer1: MutableAggregationBuffer, buffer2: Row): Unit = {

		buffer1(0) = buffer2.getAs[Long](0)
		buffer1(1) = buffer2.getAs[String](1)
		buffer1(2) = buffer2.getAs[String](2)
		buffer1(3) = buffer2.getAs[Double](3)
		buffer1(4) = buffer2.getAs[Int](4)
	}

	// This is where you output the final value, given the final value of your bufferSchema.
	override def evaluate(buffer: Row): Any = {
		buffer
	}
}

object KeepRowWithMaxAge_1 {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("KeepRowWithMaxAge_1")
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

    val predicate = $"age" < 0

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
    val filtered = exploded.filter($"age" < 0)

    val result = filtered.count()
    val runtime = (System.nanoTime() - t0) / 1e9


    val t0_pre = System.nanoTime()

    val df_pre = df.filter(predicate)
    val out_pre = df_pre.groupBy($"money").agg(udaf(df.columns.map(col): _*).as("row"))
    val exploded_pre = out_pre.select($"row.*")
    val filtered_pre = exploded_pre.filter($"age" =!= 0)

    val result_pre = filtered_pre.count()
    val runtime_pre = (System.nanoTime() - t0_pre) / 1e9
    // println(f"${(runtime - runtime_pre) / runtime * 100}%.2f%%")
    println(f"${(runtime - runtime_pre) / runtime * 100}%.2f")

    spark.stop()
  }
}
