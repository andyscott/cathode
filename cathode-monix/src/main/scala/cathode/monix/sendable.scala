/*
 * Cathode [cathode-monix]
 */

package cathode
package monix

import akka.actor._

import _root_.monix.eval.Task
import _root_.monix.execution.Scheduler

import scala.concurrent.ExecutionContext
import scala.util.{ Success, Failure }

import arrow.Sendable
import exported.Exported

// format: OFF

trait MonixSendableInstances {
  implicit def sendTaskWithExecutionContext(
    implicit ec: ExecutionContext
  ): Exported[Sendable[Task]] = Exported(MonixSendableInstances.sendTask(ec))

  implicit def sendTaskWithScheduler(
    implicit s: Scheduler
  ): Exported[Sendable[Task]] = Exported(MonixSendableInstances.sendTask(s))
}

object MonixSendableInstances {

  def sendTask(executionContext: ExecutionContext): Sendable[Task] =
    sendTask(Scheduler(executionContext))

  def sendTask(scheduler: Scheduler): Sendable[Task] =
    new Sendable[Task] {
      def send[A](ref: ActorRef, task: Task[A]): Unit = {
        val cancelable = task.runAsync(_ match {
          case Success(r) ⇒ ref ! r
          case Failure(f) ⇒ ref ! Status.Failure(f)
        })(scheduler)
      }
    }
}
