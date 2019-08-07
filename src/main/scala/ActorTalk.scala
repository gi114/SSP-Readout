import akka.actor.Actor

import scala.collection.mutable.ListBuffer

case class BinStats(map: Map[Int, ListBuffer[Int]])

class ActorTalk extends Actor {

  override def receive: Receive = {
    case BinStats(map) => display(map)
  }


  def display(map: Map[Int, ListBuffer[Int]]) = {
    map.foreach(m => {
      println("Bin: " + m._1)
      m._2.foreach(e => print(e + " -- "))
      println()
    })
  }

}
