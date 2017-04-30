package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("when 0F: should return -17.78C")  {
    val zero : Double = 0.toDouble
    val result : Double = Extraction.fahrenheitToCelsius(zero)
    assert(result === -17.78)
  }

  test("when 10F: should return -12.22C")  {
    val f : Double = 10
    val result = Extraction.fahrenheitToCelsius(f)
    assert(result === -12.22)
  }


  test("when 20F: should return -6.67C")  {
    val f : Double = 20
    val result = Extraction.fahrenheitToCelsius(f)
    assert(result === -6.67)
  }

}