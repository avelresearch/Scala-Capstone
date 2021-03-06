package observatory

import com.sksamuel.scrimage.{Image, RGBColor}
import observatory.Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val Pi: Double = 3.14159265359
  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    Location(
      math.toDegrees( math.atan( math.sinh( Pi * (1.0 - 2.0 * y / (1 << zoom) ) ) ) ),
      x / (1 << zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val width = 256
    val height = 256

    val prePixels = (0 until 256 * 256).par.map( pos =>
    {

      val dx : Int = (pos % width) / width + x

      val dy : Int = (pos / height) / height + y

      pos -> interpolateColor( colors,  predictTemperature( temperatures, tileLocation( zoom, dx, dy ) ) )

    })

    val pixels = prePixels.map(p => RGBColor(p._2.red, p._2.green, p._2.blue, 127).toPixel )

    Image(width, height, pixels.toArray )
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {

    for {
      (year, data) <- yearlyData
      z <- 0 to 3
      x <- 0 until 1 << z
      y <- 0 until 1 << z
    } generateImage(year, z, x, y, data)

  }

}
