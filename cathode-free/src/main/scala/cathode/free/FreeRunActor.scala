/*
 * Cathode [cathode-free]
 */

package cathode
package free

import cats._
import cats.free._

import akka.actor._
import scala.concurrent.Future

object FreeRunActor {
  def props[S[_]](interp: S ~> Future): Props = Props(new FreeRunActor(interp))
}

final class FreeRunActor[S[_]](interp: S ~> Future) extends Actor {
  import `cathode-internal`.FreeInternal.{ Pure, Suspend, FlatMapped }

  import context.dispatcher

  final override def receive: Actor.Receive = {
    case msg ⇒
      context.become(flatMapping(Nil, sender))
      self ! msg
  }

  def flatMapping(fs: List[Any ⇒ Any], target: ActorRef): Actor.Receive = {

    def handleValue(a: Any): Unit = fs match {
      case f :: tail ⇒
        context.become(flatMapping(tail, target))
        self ! f(a)

      case Nil ⇒ target ! a
    }

    _ match {
      case Pure(a) ⇒
        handleValue(a)

      case Suspend(sa) ⇒
        val _ = interp(sa.asInstanceOf[S[_]]).map(handleValue)

      case FlatMapped(c, f) ⇒
        context.become(flatMapping(f :: fs, target))
        self ! c
    }

  }
}
