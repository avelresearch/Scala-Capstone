package observatory

import java.time.LocalDate

import observatory.Extraction.spark
import org.apache.spark.sql.DataFrame
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

  test("when read temprature '2015': should correct data") {

    val stations : DataFrame = spark.read.csv( Extraction.getClass.getResource("/stations_.csv").getPath )

    val temperatures : DataFrame = spark.read.csv( Extraction.getClass.getResource("/2015_.csv").getPath )

    val result = Extraction.locateTemparure(2015, stations, temperatures)

    var shouldBe = Seq( (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
                        (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
                        (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
                      )
    assert( result === shouldBe)
  }

  test("when calculate avg temprature '2015': should return correct data") {

    val stations : DataFrame = spark.read.csv( Extraction.getClass.getResource("/stations_.csv").getPath )

    val temperatures : DataFrame = spark.read.csv( Extraction.getClass.getResource("/2015_.csv").getPath )

    val result2 = Extraction.locateTemparure(2015, stations, temperatures)

    val result = Extraction.locationYearlyAverageRecords(result2).toList

    val shouldBe = Seq( (Location(37.358, -78.438), 1.0), (Location(37.35, -78.433), 27.3) ).toList

    assert( result === shouldBe)
  }

}