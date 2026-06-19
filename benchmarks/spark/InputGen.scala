// InputGen.scala  – universal random‑data synthesiser + DurationMin demo
// Tested with Spark 4.0.0‑preview

import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.expressions.UserDefinedFunction
import org.apache.spark.unsafe.types.CalendarInterval

import scala.util.Random

/* ===========================================================================
 * 1.  Generic synthesiser
 * ======================================================================== */
object InputGen {

  /** Helper UDF: CalendarInterval(0,0,µ) */
  val mkInterval: UserDefinedFunction =
    udf((µ: Long) => new CalendarInterval(0, 0, µ))

  /* -----------------------------------------------------------------------
  *  generate – resilient to always‑true / always‑false predicates
  * --------------------------------------------------------------------- */
  def generate(
      spark: SparkSession,
      schema: StructType,
      predicate: Column,
      rows: Long,
      selectivity: Double,
      colOverrides: Map[String, Column] = Map.empty,
      seed: Option[Long] = None,
      // chunkRows: Long = 1_000_000L,
      stagnationLimit: Int = 12,          // NEW  (≈ 12 × 1 M rows by default)
      maxChunks: Int = 500                // NEW  absolute hard stop
    ): DataFrame = {

    require(selectivity > 0.0 && selectivity < 1.0,
      s"--selectivity $selectivity must be in (0,1)")

    val chunkRows = math.min(1_000_000L, rows)

    seed.foreach(Random.setSeed)

    /* ---------- per‑type default column generators -------------------- */
    def defaultGen(f: StructField): Column = f.dataType match {

      /* primitives already covered … */
      case IntegerType            => (rand() * 1_000).cast("int")
      case LongType               => (rand() * 1_000_000L).cast("long")
      case FloatType | DoubleType => rand()
      case StringType             => expr("substring(md5(cast(rand() as string)),1,8)")
      case BooleanType            => rand() < 0.5
      case CalendarIntervalType   => mkInterval((rand() * 1_000).cast("long"))
      case TimestampType          =>
        expr("timestamp_seconds(CAST(rand()*1735680000 AS BIGINT))") // ~ 55 years

      /* NEW ───────────────────────────────────────────────────────────── */
      case dt: DecimalType =>
        // generate a positive value in [0 , 10^(precision‑scale)) then cast
        val intDigits = math.pow(10, dt.precision - dt.scale)
        (rand() * lit(intDigits)).cast(dt)

      case DateType =>
        expr("date_add('1970-01-01', CAST(rand()*20000 AS INT))")     // ~ 55 years

      case ShortType =>
        (rand() * 1000).cast("smallint")

      /* ----------------------------------------------------------------- */
      case other =>
        throw new IllegalArgumentException(
          s"No default generator for type $other - use colOverrides.")
    }

    /* ---------- helper to build one random chunk ---------------------- */
    def buildChunk(n: Long): DataFrame = {
      var df = spark.range(n).select(lit(1).as("dummy"))
      schema.fields.foreach { f =>
        val exprCol = colOverrides.getOrElse(f.name, defaultGen(f)).as(f.name)
        df = df.withColumn(f.name, exprCol)
      }
      df.drop("dummy").select(schema.fieldNames.map(col): _*)
    }

    /* ---------- accumulation loop ------------------------------------- */
    val wantPass = math.round(rows * selectivity)
    val wantFail = rows - wantPass

    var havePass, haveFail          = 0L
    var pass, fail                  =
      spark.createDataFrame(spark.sparkContext.emptyRDD[Row], schema)

    var stagnantPass, stagnantFail  = 0          // NEW
    var chunksGenerated             = 0          // NEW

    while (havePass < wantPass || haveFail < wantFail) {

      if (chunksGenerated >= maxChunks)
        throw new IllegalArgumentException(
          s"Stopped after $maxChunks chunks - predicate too selective?")

      val chunk   = buildChunk(chunkRows)
      val good    = chunk.filter(predicate)
      val bad     = chunk.except(good)

      val needP   = (wantPass - havePass).toInt max 0
      val needF   = (wantFail - haveFail).toInt max 0

      val addP    = good.limit(needP)
      val addF    = bad .limit(needF)

      /* ---- stagnation detection (NEW) ---- */
      if (needP > 0 && addP.isEmpty) stagnantPass += 1 else stagnantPass = 0
      if (needF > 0 && addF.isEmpty) stagnantFail += 1 else stagnantFail = 0

      if (stagnantPass >= stagnationLimit)
        throw new IllegalArgumentException(
          s"Predicate appears to **always pass** " +
          s"with current column generators - add a colOverride or relax selectivity.")

      if (stagnantFail >= stagnationLimit)
        throw new IllegalArgumentException(
          s"Predicate appears to **never pass** " +
          s"with current column generators - add a colOverride or relax selectivity.")

      havePass += addP.count()
      haveFail += addF.count()

      pass = pass.unionByName(addP)
      fail = fail.unionByName(addF)

      chunksGenerated += 1
    }

    /* ---------- shuffle once to avoid ordering bias ------------------ */
    pass.unionByName(fail)
        .withColumn("__rnd__", rand())
        .orderBy("__rnd__")
        .drop("__rnd__")
  }
}
