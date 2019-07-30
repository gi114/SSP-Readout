import scala.collection.mutable.{ListBuffer, Map}

class Constructs() extends Configuration {

  var runTime = 0

  def run(): Unit = {
    val r = new scala.util.Random
    for (_ <- 0 to agentsNumber) {

      runTime += 1
      var x = 0; var y = 0; var E = 0 //starting point
      var d = math.round(r.nextFloat()); var T: Double = 0

      while (y < maxJunction) {

        val rand = r.nextFloat()

        /** SPLIT JUNCTION **/
        if (split_junctions.contains(y)) {  // split junction, 50-50
          if (rand < 0.5) {
            y = y + 1; d = DOWN; T = T + DOWN_time
          }
          else {
            y = y + 1; x = x + 1; d = DIAGONAL; T = T + DIAGONAL_time
          }
        /** PASS JUNCTION **/
        } else { // pass junction
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
      }

      //update outputs

      if (E == 0) {
        exit_count(x) = exit_count(x) + 1
      } else {
        exit_count_errors(x) = exit_count_errors(x) + E
      }

      exitsCount(x) = update(exitsCount(x))
      totalTime = totalTime + T.toLong

      if (isMultiple) updateBins
    }

    //TODO: Display bins using scalaJS
    map.foreach(elem => {
      println("Bin: " + elem._1)
      elem._2.foreach(exits => print(" -- " + exits))
      println()
    })

    //exitsCount.foreach(e => println(e._1, e._2))
    //println(totalTime)

  }

  def update(value: (Int, Int)): (Int, Int) = (value._1, value._2 + 1) //keeping the same exit but updating the count of agents

  /**
    * Checks on the multiple of the current total time with respect to the number of seconds to wait for the next binning
    * The newMultiple value is calculated as Int, hence we receive only the value before the decimal point, hence the whole part
    * In such way, when a new number is present as whole part, we update the currentMultiple with the
    * new value and check for binning updates
    *
    * @return Boolean
    */

  def isMultiple: Boolean = {
    // Why make it an Integer? You only interested in whole number before decimal point to know how many times you passed the binCycle
    val newMultiple = (totalTime/binCycle).toInt

    // When you obtain a new multiple, new time you passed the binCycle, then you update your bins
    /** Condition for binning **/
    val condition: Boolean = newMultiple > currentMultiple
    if (condition) {
      currentMultiple = newMultiple
      condition
    } else !condition
  }

  /**
    * Perform the following operations:
    * - Get mutable map
    * - Checks for how many bins are needed based on multiples of binSize
    * - Create necessary bins
    * - Update bins
    * @return mutable Map with updates bins
    */

  def updateBins: Unit = {

    /** ask the question, in what range of multiples are they? try minValue/binSize, catch exceptions in case.
       the division will tell you into what bin that exit will fall **/

    exitsCount.foreach {
      e => elementUpdate(e)
    }

    //empty your bins for your next cycle
    if (runTime != agentsNumber + 1) {
      map.clear()
    }
  }

  /**
    * Finds the bin corresponding to current number of agents at the given exit e._1,
    * takes the current list of exits belonging to that bin,
    * adds the current exit to the bin
    * @param e tuple of exit number, e._1 and total number of agents at exit (for the moment) e._2
    */

  def elementUpdate(e: (Int, Int)): Unit = {
    val binKey = getBin(e._2) //what bin key it will fall into

    /**check if binKey exists in map and add it if not**/
    if (!map.contains(binKey)) map.addOne((binKey, ListBuffer.empty[Int]))

    val mutableList = map(binKey)
    mutableList.append(e._1)
  }

  def getBin(exitValue: Int): Int = exitValue/binSize


}



