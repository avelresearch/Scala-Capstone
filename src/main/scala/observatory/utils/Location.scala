package observatory.utils

import java.time.LocalDate
import observatory.Location
import Temperature.FahrenheitToCelsius




case class Celsius(degrees : Double)

case class Fahrenheit( inVal: String)
{
  val degrees : Double = if (inVal.isEmpty) 9999.9 else inVal.toDouble
}

case class Local(year: Int, month: String, day: String, lat: String, lon: String, degrees: Fahrenheit)
{
  def numParse(s: String): Double = s.toDouble

  val localDate = LocalDate.parse(s"$year-$month-$day")

  val location = Location( numParse(lat), numParse(lon) )

  val celsius : Celsius = degrees

}
