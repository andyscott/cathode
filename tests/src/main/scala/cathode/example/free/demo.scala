/*
 * Cathode [tests]
 */

package cathode
package example
package free

import cats._
import cats.data._
import cats.free._

import akka.actor._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
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
      StdIn.readLine()
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

    val program = for {
      _ ← io.println("Hello!")
      key1 ← io.readln("What key?")
      value1 ← io.readln("What value?")
      _ ← storage.set(key1, value1)
      res1 ← storage.get(key1)
      key2 ← io.readln("What key, again?")
      res2 ← storage.get(key2).map(_ getOrElse "<<nada>>")
      _ ← io.println(s"you looked up $res2")
      _ ← io.println("thanks for playing")

    } yield res2

    val system = ActorSystem("cathode")

    val storageRef: ActorRef = system.actorOf(StorageActor.props)
    val interpreter = IOOp.interpreter or AskFunctionK[StorageActor.Op](storageRef, 10.seconds)

    val res0 = FreeRun[Program](system, 100.seconds, program, interpreter)
    val res1 = Await.result(res0, Duration.Inf)

    println("Result is " + res1)

    val terminate = system.terminate()
  }

}
