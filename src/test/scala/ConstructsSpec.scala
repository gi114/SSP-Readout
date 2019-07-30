import org.scalatest.{BeforeAndAfterEach, FreeSpec}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.Matchers._

class ConstructsSpec extends FreeSpec with MockitoSugar with BeforeAndAfterEach with Configuration {

  private val mockConstructs = mock[Constructs]

  override def beforeEach(): Unit = {
    mockConstructs.run()
  }


  "When running the simulation"  - {
    "When releasing binning results" - {
      "Locate exits into the right bin" in {

        exitsCount.map(elem => {
          val exitValue = elem._2
          val bin = mockConstructs.getBin(exitValue)
          if (exitValue < binSize) bin shouldEqual 0
          else bin shouldEqual (exitValue/binSize - 1)
        })

      }
    }
  }
}
