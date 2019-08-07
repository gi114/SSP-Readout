import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}

trait Configuration {

  /**
    * Configures mutable and immutable variables and data structures
    */

  val splitJunctions: PartialFunction[Int, List[Int]] = new PartialFunction[Int, List[Int]] {

    val maxList: IndexedSeq[Int] = IndexedSeq(0, 2, 5, 10, 17, 28, 41, 58, 77, 100, 129, 160, 197, 238, 281, 328, 381, 440, 501, 568, 639, 712, 791, 874, 963, 1060, 1161, 1264, 1371)

    def apply(netSize: Int): List[Int] = netSize match {
      case n if maxList.contains(n) => {
        val i = maxList.indexOf(n)
        maxList.slice(0, i).toList
      }
      case _ => {
        println("Not supported value")
        maxList.slice(0, 1).toList
      }
    }

    override def isDefinedAt(x: Int): Boolean = maxList.contains(x)

  }

  val maxJunction = 328                                             /** Input: Largest Junction In The Network, This Is How The Network Is Called e.g. 328 network **/

  val agentsNumber = 1746608

  var count = 0                                                     /**Counting the loops performed**/

  val netSize: Int = maxJunction + 1                                /** Network Size **/

  val exitsCount: ArrayBuffer[(Int, Int)] = ArrayBuffer.range(0, netSize) zip new Array[Int](netSize)   /** This is the main data structure holding exits numbers and corresponding number of agents
                                                                                                It is initialized with 0 counts of agents and will be filled during simulation **/

  val split_junctions: List[Int] = splitJunctions(maxJunction)      /** List of Split Junctions for the y coordinate, all others are pass junctions **/

  val DOWN = 0; val DIAGONAL = 1; val epsilon = 0.001               /** SSP rules and Input: error at pass junction **/

  val agentVelocity = 10; var totalTime: Long = 0                   /** **/

  val DOWN_time: Double = 67/agentVelocity                          /** **/

  val DIAGONAL_time: Double = 102.1/agentVelocity                   /** **/

  var currentMultiple = 0; val binSize = 10; val binCycle = 100000  /** **/

  val map: Map[Int, ListBuffer[Int]] = Map(0 -> ListBuffer.empty)   /** Holds the bin number, hence 0 for 0-9, 1 for 10-19, 2 for 20-29 and so on, and the list of exits that
                                                                        are categorized to belong to the corresponding bin based on the current number of agents. It is dynamically updated **/

  val exit_count = new Array[Int](netSize)                          /** Initializing output of correct exits count **/

  val exit_count_errors = new Array[Int](netSize)                   /** Initializing output of wrong exits count **/

  /** Alternative Methods**/
  val primes: List[Int] = List(5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47) /** Primes in network e.g. in net size 328, cardinality 15 **/

  val sumPrimes: List[Int] = sum(primes)                              /** List of Pass Junctions for coordinate y **/

  val N: Int = sumPrimes.max                                                     /** Network Size **/

  def sum(xs: List[Int]): List[Int] = {
    @tailrec
    def inner(xs: List[Int], accum: Int, res: ListBuffer[Int]): ListBuffer[Int] = {
      xs match {
        case x :: tail => {
          val acc = accum + x
          res.append(acc)
          inner(tail, acc, res)
        }
        case Nil => res
      }
    }
    val accum = 1
    List(accum) ::: inner(xs, accum, ListBuffer[Int]()).toList
  }
}



