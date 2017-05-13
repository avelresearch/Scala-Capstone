package observatory

import scala.math._
import scala.language.postfixOps

case class Latitude(degrees : Double)

case class Longitude(degrees : Double)

case class Radian(degrees : Double)

case class Location(lat: Double, lon: Double) {

  import  observatory.utils.Temperature.{LatitudeToRadians, LongitudeToRadians}

  val point: Point = Point( Latitude(lat), Longitude(lon) )
}

case class Color(red: Int, green: Int, blue: Int)

case class Point(x: Radian, y: Radian)
{
  val earthRadius : Double = 6372.8 * 1000

  lazy val location: Location = Location( x.degrees , y.degrees )

  def distance(point: Point): Double = earthRadius * getGreatCircleDistance(point)

  def getGreatCircleDistance(other: Point): Double = {

    val dx = abs(other.x.degrees - x.degrees)

    val dy = abs(other.y.degrees - y.degrees)

    val a =  pow( sin(dx / 2), 2) + cos(x.degrees) * cos(other.x.degrees) * pow(sin(dy / 2), 2)

    2 * atan2( sqrt(a), sqrt(1 - a) )
  }

}
