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

import cathode.reflect.HoleClassTag

/** Companion for [[AskFunctionK]]
  *
  * @author Andy Scott
  */
object AskFunctionK {

  def apply[F[_]: HoleClassTag](
    peer: ActorRef,
    timeout: Timeout
  ): AskFunctionK[F] = new AskFunctionK(peer, timeout)
}

/** A natural transformation from `F` to `Future`, backed by asking
  * `peer` to handle the entire value of `F[A]`
  *
  * @author Andy Scott
  */
class AskFunctionK[F[_]](peer: ActorRef, timeout: Timeout)(
    implicit
    hole: HoleClassTag[F]
) extends (F ~> Future) {

  override def apply[A](fa: F[A]): Future[A] =
    peer.ask(fa)(timeout).mapTo[A](hole.classTagA(fa))

}
