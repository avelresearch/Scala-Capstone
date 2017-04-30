package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {


  def fahrenheitToCelsius(f : Double): Double = {

    val result : Double = (f - 32.toDouble) * ( 5.toDouble / 9.toDouble )

    BigDecimal( result ).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */


  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {


    ???
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

}
