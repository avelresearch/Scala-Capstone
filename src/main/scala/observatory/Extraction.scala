package observatory

import java.time.LocalDate

import observatory.utils.{Fahrenheit, Local}
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions.{col, udf}

/**
  * 1st milestone: data extraction
  */
object Extraction extends {

  lazy val spark = SparkSession.builder.master("local").getOrCreate()

  lazy val identifier = udf( (stn: String, wban: String) => (stn, wban))

  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stations : DataFrame = spark.read.csv( Extraction.getClass.getResource(stationsFile).getPath )

    val temperatures : DataFrame = spark.read.csv( Extraction.getClass.getResource(temperaturesFile).getPath )

    locateTemperatures(year, stations, temperatures)
  }

  def locateTemperatures(year: Int, stations : DataFrame, temperatures: DataFrame) : Iterable[(LocalDate, Location, Double)] = {

    val stationsDF = stations.toDF("stn", "wban", "latitude", "longitude")
      .na.fill("", Seq("stn", "wban") )
        .withColumn("identifier", identifier( col("stn"), col("wban") ) )

    val temperaturesDF = temperatures
      .toDF("stn", "wban", "month", "day", "degrees")
        .na.fill("", Seq("stn", "wban") )
          .withColumn("identifier", identifier( col("stn"), col("wban") ))

    val joinedData = temperaturesDF.join(stationsDF, "identifier")

    val resultDF = joinedData
      .select("latitude", "longitude", "month", "day", "degrees")
        .na.drop()
          .map( row => Local(   year,
                            row.getAs[String]("month"),
                            row.getAs[String]("day"),
                            row.getAs[String]("latitude"),
                            row.getAs[String]("longitude"),
                            Fahrenheit(  row.getAs[String]("degrees") ) ) )

    resultDF
      .collect()
        .par
          .toStream
            .map(t => ( t.localDate, t.location, t.celsius.degrees ) )
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] =
    records
        .groupBy(_._2)
            .mapValues(x =>
                x.foldLeft(0.0)( (temp, loc) => temp + loc._3 ) / x.size )


}
