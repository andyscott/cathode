/*
 * Cathode [cathode-core]
 */

package cathode
package arrow

import cats.~>

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future

/** Companion for [[AskFunctionK]]
  *
  * @author Andy Scott
  */
object AskFunctionK {
  def apply[F[_]](
    peer: ActorRef,
    timeout: Timeout
  )(implicit ev: F[_] =:!= Nothing): AskFunctionK[F] = new AskFunctionK(peer, timeout)
}

/** A natural transformation from `F` to `Future`, backed by asking
  * `peer` to handle the entire value of `F[A]`
  *
  * @author Andy Scott
  */
class AskFunctionK[F[_]](peer: ActorRef, timeout: Timeout) extends (F ~> Future) {
  import scala.concurrent.`cathode-internal`.FutureHelper._

  override def apply[A](fa: F[A]): Future[A] =
    peer.ask(fa)(timeout).map(_.asInstanceOf[A])(InternalCallbackExecutor)

}
