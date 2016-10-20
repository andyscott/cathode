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
  ): AskFunctionK[F] = new AskFunctionK(peer, timeout)
}

/** A natural transformation from `F` to `Future`, backed by asking
  * `peer` to handle the entire value of `F[A]`
  *
  * @author Andy Scott
  */
class AskFunctionK[F[_]](peer: ActorRef, timeout: Timeout) extends (F ~> Future) {

  import scala.reflect._

  private[this] val cta = classTag[Any]

  override def apply[A](fa: F[A]): Future[A] =
    peer.ask(fa)(timeout).mapTo[A](cta.asInstanceOf[ClassTag[A]])

}
