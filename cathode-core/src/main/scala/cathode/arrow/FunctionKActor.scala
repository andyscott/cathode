/*
 * Cathode [cathode-core]
 */

package cathode
package arrow

import akka.actor._
import akka.pattern.pipe

import cats._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.reflect.ClassTag

import exported.Exported

/** An actor who's receive body is backed by a natural transformation
  * `interp` from `F` to `G`
  *
  * @author Andy Scott
  */
class FunctionKActor[F[_], G[_]](interp: F ~> G, send: FunctionKActor.Send[G])(
    implicit
    evF: ClassTag[F[_]]) extends Actor {

  def this(interp: F ~> G)(implicit
    evF: ClassTag[F[_]],
    evSend: FunctionKActor.Send[G]) = this(interp, evSend)

  final override def receive: Actor.Receive = {
    case evF(fa) ⇒ send.send(sender, interp(fa))
  }
}

/** Companion for [[FunctionKActor]]]
  *
  * @author Andy Scott
  */
object FunctionKActor {

  def props[F[_], G[_]: Send](f: F ~> G)(implicit evF: ClassTag[F[_]]): Props =
    Props(new FunctionKActor(f))

  trait Send[F[_]] {
    def send[A](ref: ActorRef, msg: F[A]): Unit
  }

  object Send extends LowPrioritySendInstances {
    def apply[F[_]](implicit ev: Send[F]): Send[F] = ev

    implicit val sendConstant: Send[Id] = new Send[Id] {
      def send[A](ref: ActorRef, constant: A): Unit = {
        ref ! constant
      }
    }

    implicit def sendFuture(implicit ec: ExecutionContext): Send[Future] = new Send[Future] {
      def send[A](ref: ActorRef, future: Future[A]): Unit = {
        pipe(future)(ec).to(ref)
        ()
      }
    }

  }

  private[cathode] sealed trait LowPrioritySendInstances {
    implicit def importedSend[F[_]](
      implicit
      exported: Exported[Send[F]]
    ): Send[F] = exported.instance
  }

}
