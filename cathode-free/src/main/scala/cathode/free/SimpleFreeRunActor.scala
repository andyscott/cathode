/*
 * Cathode [cathode-free]
 */

package cathode
package free

import cats._
import cats.free._

import akka.actor._
import scala.concurrent.Future

object SimpleFreeRunActor {
  def props[S[_]](interp: S ~> Future): Props = Props(classOf[SimpleFreeRunActor], interp)
}

/** An actor to evaluate `Free` monads.
  *
  * This actor is stateful. A new actor needs to be created
  * for each evaluation of a `Free` monad.
  *
  * TODO: Optimize interaction with interpreters
  * TODO: Add support for timing out
  *
  */
final class SimpleFreeRunActor(interp: Id ~> Future) extends Actor {
  import `cathode-internal`.FreeInternal.{ Pure, Suspend, FlatMapped }
  import context.dispatcher

  final override def receive: Actor.Receive = {
    case msg ⇒
      context.become(loop(Nil, sender))
      self ! msg

  }

  private[this] final def loop(fs: List[Any ⇒ Any], s: ActorRef): Actor.Receive = {
    case Pure(a) ⇒
      pureValue(a, fs, s)

    case Suspend(sa) ⇒
      val _ = interp(sa).map[Unit](a ⇒ pureValue(a, fs, s))

    case FlatMapped(c, f) ⇒
      context.become(loop(f :: fs, s))
      self ! c
  }

  private[this] final def pureValue(a: Any, fs: List[Any ⇒ Any], s: ActorRef): Unit =
    fs match {
      case f :: tail ⇒
        context.become(loop(tail, s))
        self ! f(a)
      case Nil ⇒
        s ! a
    }

}
