trait Constructable extends Configuration {

  var x: Int = 0; var y: Int = 0
  var E: Int = 0; var d: Int = 0
  var T: Double = 0

  val r = new scala.util.Random

  var runTime = 0

  def clearCount(): Unit = {
    x = 0; y = 0
    E = 0; T = 0
    d = math.round(r.nextFloat())
  }

  def processSplitJunction(rand: Float): Unit = {
    if (rand < 0.5) {
      y = y + 1; d = DOWN; T = T + DOWN_time
    }
    else {
      y = y + 1; x = x + 1; d = DIAGONAL; T = T + DIAGONAL_time
    }
  }

  def processPassJunction(rand: Float): Unit = {
    if (d == DOWN && rand > epsilon) {
      y = y + 1; T = T + DOWN_time
    }
    else if (d == DOWN && rand < epsilon) {
      y = y + 1; x = x + 1; d = DIAGONAL; E = 1; T = T + DIAGONAL_time
    } else if (d == DIAGONAL && rand > epsilon) {
      y = y + 1; x = x + 1; T = T + DIAGONAL_time
    } else if (d == DIAGONAL && rand < epsilon) {
      y = y + 1; d = DOWN; E = 1; T = T + DOWN_time
    }
  }

  def isNextStep: Boolean = y < maxJunction

  def updateCounts(): Unit = {
    if (E == 0) {
      exit_count(x) = exit_count(x) + 1
    } else {
      exit_count_errors(x) = exit_count_errors(x) + E
    }
  }

}
