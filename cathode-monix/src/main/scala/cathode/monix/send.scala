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

import arrow.FunctionKActor.Send
import exported.Exported

// format: OFF

trait MonixSendInstances {
  implicit def sendTaskWithExecutionContext(
    implicit ec: ExecutionContext
  ): Exported[Send[Task]] = Exported(MonixSendInstances.sendTask(ec))

  implicit def sendTaskWithScheduler(
    implicit s: Scheduler
  ): Exported[Send[Task]] = Exported(MonixSendInstances.sendTask(s))
}

object MonixSendInstances {

  def sendTask(executionContext: ExecutionContext): Send[Task] =
    sendTask(Scheduler(executionContext))

  def sendTask(scheduler: Scheduler): Send[Task] =
    new Send[Task] {
      def send[A](ref: ActorRef, task: Task[A]): Unit = {
        val cancelable = task.runAsync(_ match {
          case Success(r) ⇒ ref ! r
          case Failure(f) ⇒ ref ! Status.Failure(f)
        })(scheduler)
      }
    }
}
