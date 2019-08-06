import akka.actor.Actor

import scala.collection.mutable.ListBuffer

case class BinStats(map: Int)

class ActorTalk extends Actor with Configuration {

  override def receive: Receive = {
    case BinStats(map) => display(map)
  }


  def display(map: Int) = {
    print(map)
  }

}
