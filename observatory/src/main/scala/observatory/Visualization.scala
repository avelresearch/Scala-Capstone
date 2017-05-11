package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val p : Int = 2

  // Assume all distance in meters
  val distanceThreshold = 1000.0

  type TemperatureColor = (Int, Color)
  type ValueColor = (Double, Color)

  // For the reference only
  val temperatureColors: List[TemperatureColor] = List(
    (60, Color(255, 255, 255) ),
    (32, Color(255, 0, 0) ),
    (12, Color(255, 255, 0) ),
    (0, Color(0, 255, 255) ),
    (-15, Color(0, 0, 255) ),
    (-27, Color(255, 0, 255) ),
    (-50, Color(33, 0, 107) ),
    (-60, Color(0, 0, 0) )
  )

  def round(v : Double) : Int = BigDecimal(v).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt

  def distanceBetweenLocations(x: Location, y: Location) : Double = {

    val earthRadius : Double = 6371000

    val deltaLong = math.abs(x.lon.toRadians - y.lon.toRadians)

    val partA : Double = math.sin(x.lat.toRadians) * math.sin(y.lat.toRadians)

    val partB : Double = math.cos(x.lat.toRadians) * math.cos(y.lat.toRadians) * math.cos(deltaLong.toRadians)

    val deltaInRadians : Double = math.acos(partA + partB)

    val result = earthRadius * deltaInRadians

    val distance = BigDecimal(result).setScale(2, BigDecimal.RoundingMode.HALF_DOWN).toDouble

    if (distance > 1) distance else 0
  }

  def inverseDistance(x: Location, temperatures: Iterable[(Location, Double)] ) : Double = {

    val (nominator, denominator ) = temperatures

      .par.map( y => ( ( 1 / math.pow( distanceBetweenLocations(x, y._1 ), p ) ), y._2) )

      .par.map( z => (z._1 * z._2, z._1) )

      .reduce( (x, y) => (x._1 + y._1, x._2 + y._2) )

    nominator / denominator
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double =
    temperatures.find(t => t._1 == location) match { case Some(r) => r._2 case None => inverseDistance(location, temperatures) }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {


    def interpolate(min: Double, max: Double, value: Double, colorValueMin: Int, colorValueMax: Int): Int =
    {
      val f = (value - min) / (max - min)

      round(colorValueMin + (colorValueMax - colorValueMin) * f)
    }

    def linearInterpolation(a: Option[ValueColor], b: Option[ValueColor], value: Double): Color = (a, b) match {
      case ( Some((pa, pA) ), Some(( pb, pB))) =>
      {
        Color( interpolate(pa, pb, value, pA.red, pB.red),  interpolate(pa, pb, value, pA.green, pB.green),  interpolate(pa, pb, value, pA.blue,  pB.blue)
        )
      }
      case (Some(pA), None) => pA._2
      case (None, Some(pB)) => pB._2
      case _ => Color(0, 0, 0)
    }

    // Match with existing color scheme first
    points.find( point => point._1 == value) match
    {
      case Some( (_, color) ) => color

      case None =>
      {
        val sorted = points.toList.sortBy(x => x._1)

        val (l, g) = sorted.partition( point => point._1 < value)

        linearInterpolation( l.lastOption, g.headOption, value)
      }
    }

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

      val x: Int = pos % WIDTH
      val y: Int = pos / WIDTH

      Location(90 - (y * heightFactor), (x * widthFactor) - 180)
    }

    def getPixel(latitude: Double, longitude: Double) : Pixel = {

      val location = Location(latitude, longitude)

      val temperature = predictTemperature( temperatures, location )

      val color = interpolateColor(colors, temperature)

      Pixel(color.red, color.green, color.blue, 255)
    }

    val pixels = (0 until WIDTH * HEIGHT)
      .map( p => pixelToLocation(p) )
      .map( location => getPixel( location.lat, location.lon) )
      .toArray

    Image(WIDTH, HEIGHT, pixels )
  }

}

