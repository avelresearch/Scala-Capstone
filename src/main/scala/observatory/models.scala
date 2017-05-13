package observatory

import  observatory.utils.Temperature.{LatitudeToRadians, LongitudeToRadians, RadiansToLatitude, RadiansToLongitude}
import  observatory.utils.{Latitude, Longitude, Radian}

import scala.math._

case class Location(lat: Double, lon: Double) {
  val point: Point = Point( Latitude(lat), Longitude(lon) )
}

case class Color(red: Int, green: Int, blue: Int)

case class Point(x: Radian, y: Radian)
{
  val earthRadius : Double = 6372.8 * 1000

  lazy val location: Location = Location( x.degrees , y.degrees )

  def haversineEarthDistance(point: Point): Double = earthRadius * getGreatCircleDistance(point)

  def getGreatCircleDistance(other: Point): Double = {

    val dx = abs(other.x.degrees - x.degrees)

    val dy = abs(other.y.degrees - y.degrees)

    val a =  pow( sin(dx / 2), 2) + cos(x.degrees) * cos(other.x.degrees) * pow(sin(dy / 2), 2)

    2 * atan2( sqrt(a), sqrt(1 - a) )
  }

}
