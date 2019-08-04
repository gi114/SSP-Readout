import akka.actor.Actor

import scala.collection.mutable.ListBuffer

case class BinStats(stats: Map[Int, ListBuffer[Int]])

class ActorsTalk extends Actor {

  var countMessages = 0

  override def receive: Receive = {
    case BinStats(stats) => displayStats(stats)
  }

  def displayStats(map: Map[Int, ListBuffer[Int]]) = {
    countMessages += 1
    map.foreach(elem => {
      println("Bin: " + elem._1)
      elem._2.foreach(exits => print(" -- " + exits))
      println()
    })
  }

}
