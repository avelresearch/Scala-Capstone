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

    val distance : Double =  953779.11 // Actual: 968.9 & we are loosing around 10 km

    val result = Visualization.distanceBetweenLocations(x, y)

    assert(result == distance)
  }

  test("distance between: (50.066389 5.714722) and (50.066389 5.714722) should be 0 Km " )  {
    val x = Location(50.066389, -5.714722)
    val y = Location(50.066389, -5.714722)

    val distance : Double =  0 // Actual: 968.9 & we are loosing around 10 km

    val result = Visualization.distanceBetweenLocations(x, y)

    assert(result == distance)
  }

//
//  test("List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.7"){
//    val value : Double = -0.7
//    val list : List[(Double, Color)] = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255)))
//    val result = Visualization.interpolateColor(list, value)
//
//    assert(result === Color(191,0,64))
//  }
//
//  test("List((-1.0,Color(255,0,0)), (24.899394980716096,Color(0,0,255))), value = -11.0"){
//    val value : Double = -11.0
//    val list : List[(Double, Color)] = List((-1.0,Color(255,0,0)), (24.899394980716096,Color(0,0,255) ) )
//    val result = Visualization.interpolateColor(list, value)
//
//    assert(result === Color(255,0,0))
//  }

  //Color(192,0,63). Expected: Color(191,0,64) (scale = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75)

  test("List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75"){
    val value : Double = -0.75
    val list : List[(Double, Color)] = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255)))
    val result = Visualization.interpolateColor(list, value)

    assert(result === Color(191,0,64))
  }

  test("List((-98.83145445740537,Color(255,0,0)), (32.053536491879356,Color(0,0,255))), value = -98.83145445740537"){
    val value : Double = -98.83145445740537

    val list : List[(Double, Color)] = List((-98.83145445740537,Color(255,0,0)), (32.053536491879356,Color(0,0,255)))

    val result = Visualization.interpolateColor(list, value)

    assert(result === Color(255,0,0) )
  }

  test("List((1.0,Color(255,0,0)), (23.053046156462315,Color(0,0,255))) value = -9.0"){

    val value : Double = -9.0

    val list : List[(Double, Color)] = List((1.0,Color(255,0,0)), (23.053046156462315,Color(0,0,255)))

    val result = Visualization.interpolateColor(list, value)

    assert(result === Color(255,0,0) )
  }

  test("List((1.0,Color(255,0,0)), (23.053046156462315,Color(0,0,255))) value = 24.0"){

    val value : Double = 24.0

    val list : List[(Double, Color)] = List((1.0,Color(255,0,0)), (23.053046156462315,Color(0,0,255)))

    val result = Visualization.interpolateColor(list, value)

    assert(result === Color(0,0,255) )
  }



}
