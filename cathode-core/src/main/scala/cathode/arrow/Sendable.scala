/*
 * Cathode [cathode-core]
 */

package cathode
package arrow

import cats._

import akka.actor._
import akka.pattern.pipe

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import exported._

trait Sendable[F[_]] {
  def send[A](ref: ActorRef, msg: F[A]): Unit
}

object Sendable extends LowPrioritySendableInstances {
  def apply[F[_]](implicit ev: Sendable[F]): Sendable[F] = ev

  implicit val sendableConstant: Sendable[Id] = new Sendable[Id] {
    def send[A](ref: ActorRef, constant: A): Unit = {
      ref ! constant
    }
  }

  implicit def sendableFuture(implicit ec: ExecutionContext): Sendable[Future] =
    new Sendable[Future] {
      def send[A](ref: ActorRef, future: Future[A]): Unit = {
        pipe(future)(ec).to(ref)
        ()
      }
    }

}

private[cathode] sealed trait LowPrioritySendableInstances {
  implicit def importedSendable[F[_]](
    implicit
    exported: Exported[Sendable[F]]
  ): Sendable[F] = exported.instance
}
