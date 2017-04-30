package observatory

import java.time.LocalDate

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions.{udf, _}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  lazy val spark = SparkSession.builder.master("local").getOrCreate()

  def fahrenheitToCelsius(f : Double): Double = {

    val result : Double = (f - 32.toDouble) * ( 5.toDouble / 9.toDouble )

    BigDecimal( result ).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  case class Local(year: Int, month: String, day: String, lat: String, lon: String, degrees: String)
  {
    def numParse(s: String): String = s

    val localDate = LocalDate.parse(s"$year-$month-$day")
    val location = Location( numParse(lat).toDouble, numParse(lon).toDouble )
    val celcium = fahrenheitToCelsius( degrees.toDouble )
  }

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

    locateTemparure(year, stations, temperatures)
  }

  def locateTemparure(year: Int, stations : DataFrame, temperatures: DataFrame) : Iterable[(LocalDate, Location, Double)] = {
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
      .map(row => Local(
        year,
        row.getAs[String]("month"),
        row.getAs[String]("day"),
        row.getAs[String]("latitude"),
        row.getAs[String]("longitude"),
        row.getAs[String]("degrees")
      ) )

    resultDF.collect().par.toStream.map(t => ( t.localDate, t.location, t.celcium ) )
  }

  //    {
  //      val localDate = LocalDate.parse(s"$year-$month-$day")
  //      val location = Location(lat.toDouble, lon.toDouble)
  //      val celcium = fahrenheitToCelsius( degrees.toDouble )
  //    }
  //temperatures.collect().par.toStream.map(t => ( LocalDate.of(t.year, t.month, t.day),  Location(t.lat, t.lon), t.temp ) )

  //    val stations = scala.io.Source.fromFile("/stations.csv").getLines()
  //          .map(line => line.split(",") )
  //          .map(arr => Station(arr(0), arr(1), arr(2).toDouble, arr(3).toDouble ) )


  //    val data : ListBuffer[(LocalDate, Location, Double)] = ListBuffer()
  //
  //    for(year <- 1975 to 2015)
  //    {
  //      //val localDate =
  //      val yearData =  scala.io.Source.fromFile(s"/$year").getLines()
  //        .map(line => line.split(",") )
  //        .map(arr => YearRow( arr(0), arr(1), s"$year", arr(2), arr(3), fahrenheitToCelsius( arr(4).toDouble ) ) )
  //        .map(row => (row.localDate, stations.find(s => s.identifier == row.identifier).get, row.degrees ) )
  //
  //      data ++ yearData
  //    }
  //
  //    data

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(x => x.map(y => y._3).sum / x.size )

  }



}
