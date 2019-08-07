import akka.actor.{ActorRef, ActorSystem, Props}
import scala.concurrent.duration._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.ExecutionContext
import scala.language.postfixOps

class Constructs() extends Configuration with Constructable {

  private val actorSystem = ActorSystem("BinsProcessing")

  private val actor: ActorRef = actorSystem.actorOf(Props[ActorTalk], name = "BinsProcessor")

  implicit val executionContext = ExecutionContext.Implicits.global

  def run(): ArrayBuffer[(Int, Int)] = {

    actorSystem.scheduler.scheduleWithFixedDelay(3 seconds, 5 seconds, actor, BinStats(map.toMap))

    for (_ <- 0 to agentsNumber) {

      count += 1
      runTime += 1

      clearCount()

      while (isNextStep) {

        val rand = r.nextFloat()

        /** SPLIT JUNCTION **/
        if (split_junctions.contains(y)) {  // split junction, 50-50
          processSplitJunction(rand)


        /** PASS JUNCTION **/
        } else { // pass junction
          processPassJunction(rand)
        }
      }

      //update outputs

      updateCounts()
      exitsCount(x) = update(exitsCount(x))
      totalTime = totalTime + T.toLong

      if (isMultiple) updateBins
    }

    //TODO: Display bins using scalaJS


    display()
    actorSystem.terminate()
    exitsCount
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
    if (newMultiple > currentMultiple) {
      currentMultiple = newMultiple
      true
    } else false
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

    //add to current bin if not there yet
    val mutableList = map(binKey)
    if (!mutableList.contains(e._1)) {
      mutableList.append(e._1)
      if (binKey > 0) search(binKey - 1, e._1)
    }
  }

  /**
    * Search in the previous bin
    *   if not in previous bin, just return
    *   else remove exit from previous bin
    *
    * @param previousBin bin to search where exit could have been located at previous runs
    * @param exit what exit to search for
    */

  def search(previousBin: Int, exit: Int): Unit = {
    val mutableList = map(previousBin)
    val l1 = mutableList.length
    if (mutableList.contains(exit)) {
      mutableList -= exit
      assert(mutableList.length == l1 - 1)
    }
  }

  def getBin(exitValue: Int): Int = exitValue/binSize

  def display() = {
    map.foreach(m => {
      println("Bin: " + m._1)
      m._2.foreach(e => print(e + " -- "))
      println()
    })
  }
}



