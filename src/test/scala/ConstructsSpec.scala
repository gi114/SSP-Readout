import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.Matchers._

import scala.collection.mutable.ArrayBuffer

class ConstructsSpec extends FreeSpec with MockitoSugar with BeforeAndAfterAll with Constructable with Configuration {

  private val construct = new Constructs()

  val result: ArrayBuffer[(Int, Int)] = construct.run()

  "When running the simulation"  - {
    "When releasing results" - {

      "Fill exits counts with appropriate counts" in {

        result.foreach(e => println(e._1, e._2))

        val initialized = ArrayBuffer.range(0, netSize) zip new Array[Int](netSize)
        result should not equal initialized
      }

      "Allocate exits into the right bin" in {

        result.map(elem => {
          val exitValue = elem._2
          val bin = construct.getBin(exitValue)

          // bin 0 takes exits with value from 0-9
          if (exitValue < binSize) bin shouldEqual 0
          else bin shouldEqual (exitValue/binSize)
        })

      }
    }
  }
}
