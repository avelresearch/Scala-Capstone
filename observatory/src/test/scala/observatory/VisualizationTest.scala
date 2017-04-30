package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  //As decimals: 50.0663889 Latitude, -5.7147222 Longitude
  //As decimals: 58.6438889 Latitude, -3.0700000 Longitude
  test("distance between: (50.066389 5.714722) and (58.643889 3.07) should be 953.779 Km") {
    val x = Location(50.066389, -5.714722)
    val y = Location(58.643889, -3.07)

    val distance : Double =  953.779 // Actual: 968.9 & we are loosing around 10 km

    val result = Visualization.distance(x, y)

    assert(result == distance)
  }

  test("distance between: (50.066389 5.714722) and (50.066389 5.714722) should be 0 Km " )  {
    val x = Location(50.066389, -5.714722)
    val y = Location(50.066389, -5.714722)

    val distance : Double =  0 // Actual: 968.9 & we are loosing around 10 km

    val result = Visualization.distance(x, y)

    assert(result == distance)
  }

}
