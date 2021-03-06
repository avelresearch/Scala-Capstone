package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val latitudeRange = -89 to 90
    val longitudeRange = -180 to 179

    val grid: Map[Location, Double] = {
      for {
        latitude <- latitudeRange
        longitude <- longitudeRange
      } yield Location(latitude, longitude) ->
          Visualization.predictTemperature(temperatures, Location(latitude, longitude) )
    }.toMap

    (latitude, longitude) => grid( Location(latitude, longitude) )
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {

    val grids: Iterable[(Int, Int) => Double] = temperaturess.map(makeGrid)

    (latitude, longitude) =>
    {
      val temps = grids.map(grid => grid(latitude, longitude))
      temps.foldLeft(0.0)( (t:Double, x: Double) => t + x) / temps.size
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {

    val grid = makeGrid(temperatures)

    (latitude, longitude) => grid(latitude, longitude) - normals(latitude, longitude)
  }


}

