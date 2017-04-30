package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val N : Int = 8

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

  def distance(x: Location, y: Location) : Double = {

    val pi : Double = 3.14159265358979323846264338327950288419716939937510582097494459230781640628

    def toRadian(d: Double) : Double = (d * pi / 180.toDouble)
    //val deltaLat = math.abs(x.lat - y.lat)

    val deltaLong = math.abs(x.lon.toRadians - y.lon.toRadians)

    val partA : Double = math.sin(x.lat.toRadians) * math.sin(y.lat.toRadians)

    val partB : Double = math.cos(x.lat.toRadians) * math.cos(y.lat.toRadians) * math.cos(deltaLong.toRadians)

    val delta : Double = math.acos(partA + partB)

//    val partA : Double =   math.sin( deltaLat / 2) * math.sin( deltaLat / 2)
//
//    val partB : Double = math.cos(x.lat) * math.cos(y.lat) * math.sin(deltaLong / 2) * math.sin(deltaLong / 2)
//
//    val innerSum : Double = partA + partB
//
//    val delta = 2 * math.asin( math.sqrt( innerSum ) )

    val earthRadius : Double = 6371.toDouble

    val result = earthRadius * delta

    BigDecimal(result).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    // ref:  https://en.wikipedia.org/wiki/Inverse_distance_weighting
    ???
  }

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

