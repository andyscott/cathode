/*
 * Cathode [cathode-core]
 */

package cathode
package arrow

import cats._
import akka.actor._
import scala.reflect.ClassTag

/** An actor who's receive body is backed by a natural transformation
  * `interp` from `F` to `G`
  *
  * @author Andy Scott
  */
class FunctionKActor[F[_], G[_]](interp: F ~> G)(
    implicit
    evF: ClassTag[F[_]],
    evSendable: Sendable[G]) extends Actor {

  final override def receive: Actor.Receive = {
    case evF(fa) ⇒ evSendable.send(sender, interp(fa))
  }
}

/** Companion for [[FunctionKActor]]]
  *
  * @author Andy Scott
  */
object FunctionKActor {

  def props[F[_], G[_]: Sendable](f: F ~> G)(implicit evF: ClassTag[F[_]]): Props =
    Props(new FunctionKActor(f))

}
