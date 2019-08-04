import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.Matchers._

class ConstructsSpec extends FreeSpec with MockitoSugar with BeforeAndAfterAll with Constructable {

  private implicit val mockConstructs = mock[Constructs]
  private implicit val mockConfig = mock[Configuration]

  override def beforeAll(): Unit = {
    mockConstructs.run
  }


  "When running the simulation"  - {
    "When releasing results" - {

      "Fill exits counts with appropriate counts" in {

        exitsCount.foreach(e => println(e._1, e._2))

        val initialized = Array.range(0, netSize) zip new Array[Int](netSize)
        exitsCount should not equal initialized
      }

      "Allocate exits into the right bin" in {

        exitsCount.map(elem => {
          val exitValue = elem._2
          val bin = mockConstructs.getBin(exitValue)
          if (exitValue < binSize) bin shouldEqual 0
          else bin shouldEqual (exitValue/binSize - 2)
        })

      }
    }
  }
}
