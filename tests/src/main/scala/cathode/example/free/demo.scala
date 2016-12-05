/*
 * Cathode [tests]
 */

package cathode
package example
package free

import cats._
import cats.data._
import cats.free._
import cats.syntax.flatMap._

import akka.actor._

import scala.concurrent._
import scala.concurrent.duration._
import scala.io.StdIn

import cathode.arrow._
import cathode.free._

class IOOps[F[_]](implicit I: Inject[IOOp, F]) {
  def println(msg: ⇒ String): Free[F, Unit] = Free.inject(IOOp.Println(() ⇒ msg))
  def readln(msg: ⇒ String): Free[F, String] = Free.inject(IOOp.Readln(() ⇒ msg))
}

sealed trait IOOp[A] extends Product with Serializable
object IOOp {
  case class Println(msg: () ⇒ String) extends IOOp[Unit]
  case class Readln(msg: () ⇒ String) extends IOOp[String]

  def interpreter(implicit ec: ExecutionContext) = λ[IOOp ~> Future] {
    case Println(msg) ⇒ Future(println(msg()))
    case Readln(msg) ⇒ Future {
      println(msg())
      blocking { StdIn.readLine() }
    }
  }
}

class StorageOps[F[_]](implicit I: Inject[StorageActor.Op, F]) {
  def get(key: String): Free[F, Option[String]] =
    Free.inject(StorageActor.Get(key))
  def set(key: String, value: String): Free[F, Unit] =
    Free.inject(StorageActor.Set(key, value))
}

object StorageActor {
  sealed trait Op[A]
  case class Get(key: String) extends Op[Option[String]]
  case class Set(key: String, value: String) extends Op[Unit]

  def props: Props = Props(classOf[StorageActor])
}

class StorageActor extends Actor {
  import StorageActor._

  override def receive: Actor.Receive = state(Map.empty)

  def state(data: Map[String, String]): Actor.Receive = {
    case Get(key) ⇒
      sender ! data.get(key)
    case Set(key, value) ⇒
      println("Wahoo! " + key + " -> " + value)
      context.become(state(data + key → value))
      sender ! (()) // send the only instance of Unit
  }
}

object FreerunDemo {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    type Program[A] = Coproduct[IOOp, StorageActor.Op, A]

    val io = new IOOps[Program]
    val storage = new StorageOps[Program]

    def prompt: Free[Program, Unit] = for {
      command ← io.readln("Enter a command (:help for help)").map(_.trim)
      _ ← command match {
        case ":quit" ⇒ quitCommand
        case ":set"  ⇒ setCommand >> prompt
        case ":get"  ⇒ getCommand >> prompt
        case ":help" ⇒ helpCommand >> prompt
        case _       ⇒ badCommand(command) >> prompt
      }
    } yield ()

    def quitCommand: Free[Program, Unit] = io.println("See ya!")
    def setCommand: Free[Program, Unit] = for {
      key ← io.readln("What key?")
      value ← io.readln("What value?")
      _ ← storage.set(key, value)
    } yield ()

    def getCommand: Free[Program, Unit] = for {
      key ← io.readln("What key?")
      value ← storage.get(key).map(_ getOrElse "<<no value>>")
      _ ← io.println(s"The value of $key is $value")
    } yield ()

    def helpCommand: Free[Program, Unit] = for {
      _ ← io.println("Commands are:")
      _ ← io.println("  :get, :set, :help, :quit")
    } yield ()

    def badCommand(command: String): Free[Program, Unit] = for {
      _ ← io.println(s"Bad command $command")
      _ ← helpCommand
    } yield ()

    val program = prompt

    val system = ActorSystem("cathode")
    val storageRef: ActorRef = system.actorOf(StorageActor.props)
    val interpreter = IOOp.interpreter or AskFunctionK[StorageActor.Op](storageRef, 10.seconds)
    val res0 = SimpleFreeRun[Program](system, 100.seconds, program, interpreter)
    val res1 = Await.result(res0, Duration.Inf)
    val terminate = system.terminate()
  }

}
