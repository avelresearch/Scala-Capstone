package observatory.utils

import scala.language.implicitConversions
import scala.language.postfixOps


object Temperature {

  implicit class DoubleExtension( degrees : Double) {
    def !() = BigDecimal( degrees ).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  implicit def FahrenheitToCelsius(f: Fahrenheit) : Celsius = {

    val degrees : Double = (f.degrees - 32.toDouble) * ( 5.toDouble / 9.toDouble )

    Celsius( degrees ! )
  }

  implicit def LatitudeToRadians(lat : Latitude) : Radian = Radian( lat.degrees.toRadians )

  implicit def LongitudeToRadians(lon : Longitude) : Radian = Radian( lon.degrees.toRadians )

  implicit def RadiansToLatitude(lat: Radian) : Latitude = Latitude( lat.degrees.toDegrees )

  implicit def RadiansToLongitude(lon: Radian) : Longitude = Longitude( lon.degrees.toDegrees )

}