package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    def distanceToTemperature(temp: Iterable[(Location, Double)], location: Location): Iterable[(Double, Double)] =
      temp.map { case (other, t) => (location.point haversineEarthDistance other.point, t) }


    def inverseDistance(distanceToTemp: Iterable[(Double, Double)], power: Int): Double =
    {
      val (weightedSum, inverseWeightedSum) = distanceToTemp
        .aggregate((0.0, 0.0))(
          {
            case ((ws, iws), (distance, temp)) => {
              val w = 1 / pow(distance, power)
              (w * temp + ws, w + iws)
            }
          }, {
            case ((wsA, iwsA), (wsB, iwsB)) => (wsA + wsB, iwsA + iwsB)
          }
        )

      weightedSum / inverseWeightedSum
    }

    val predictions: Iterable[(Double, Double)] = distanceToTemperature(temperatures, location)

    predictions.find(_._1 == 0.0) match {
      case Some((_, temp)) => temp
      case _ => inverseDistance(predictions, power = 3)
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None => {
        val (sm, gr) = points.toList.sortBy(_._1).partition(_._1 < value)
        linearInterpolation( sm.reverse.headOption, gr.headOption, value)
      }
    }
  }

  def linearInterpolation(pointA: Option[(Double, Color)], pointB: Option[(Double, Color)], value: Double): Color = (pointA, pointB) match {
    case (Some((pAValue, pAColor)), Some((pBValue, pBColor))) => {
      val li = linearInterpolationValue(pAValue, pBValue, value) _
      Color(
        li(pAColor.red, pBColor.red),
        li(pAColor.green, pBColor.green),
        li(pAColor.blue, pBColor.blue)
      )
    }
    case (Some(pA), None) => pA._2
    case (None, Some(pB)) => pB._2
    case _ => Color(0, 0, 0)
  }

  def linearInterpolationValue(pointValueMin: Double, pointValueMax: Double, value: Double)(colorValueMin: Int, colorValueMax: Int): Int = {
    val factor = (value - pointValueMin) / (pointValueMax - pointValueMin)

    round(colorValueMin + (colorValueMax - colorValueMin) * factor).toInt
  }


  def distance(locA: Location, locB: Location): Double = {
    val Location(latA, lonA) = locA
    val Location(latB, lonB) = locB

    val latDistance =  toRadians( latB - latA )
    val lonDistance = toRadians(lonB - lonA)

    val a = pow(sin(latDistance / 2), 2) +
      cos(toRadians(latA)) * cos(toRadians(latB) ) *
        pow(sin(lonDistance / 2), 2)

    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    c * 6371
  }



  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    val WIDTH = 360
    val HEIGHT = 180

    def pixelToLocation(pos: Int): Location = {

      val widthFactor = 180 * 2 / WIDTH.toDouble
      val heightFactor = 90 * 2 / HEIGHT.toDouble

      val x : Double = 90 - (pos / WIDTH) * heightFactor
      val y : Double =  (pos % WIDTH) * widthFactor - 180

      Location( x , y )
    }

    def getPixel(latitude: Double, longitude: Double) : Pixel = {

      val location = Location(latitude, longitude)

      val temperature = predictTemperature( temperatures, location )

      val color = interpolateColor(colors, temperature)

      Pixel(color.red, color.green, color.blue, 255)
    }

    val pixels = (0 until WIDTH * HEIGHT)
      .map( p => pixelToLocation(p) )
      .map( location => getPixel( location.lat, location.lon ) )
      .toArray

    Image(WIDTH, HEIGHT, pixels )
  }

}

