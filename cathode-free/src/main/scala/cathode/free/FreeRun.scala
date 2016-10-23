/*
 * Cathode [cathode-free]
 */

package cathode
package free

import cats._
import cats.free._

import akka.actor._
import akka.util.Timeout

import scala.concurrent.Future

import cathode.arrow._

object FreeRun {

  def apply[S[_], A](
    system: ActorSystem, timeout: Timeout,
    free: Free[S, A], f: S ~> Future
  ): Future[A] =

    AskFunctionK[Free[S, ?]](
      system.actorOf(FreeRunActor.props[S](f)),
      timeout).apply(free)

  def apply[S[_]]: ApplySyntax[S] = new ApplySyntax[S]

  final class ApplySyntax[S[_]] private[FreeRun] {
    def apply[A](
      system: ActorSystem, timeout: Timeout,
      free: Free[S, A], f: S ~> Future): Future[A] =
      FreeRun[S, A](system, timeout, free, f)
  }
}
