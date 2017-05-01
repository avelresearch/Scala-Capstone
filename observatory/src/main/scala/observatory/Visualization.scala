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
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

