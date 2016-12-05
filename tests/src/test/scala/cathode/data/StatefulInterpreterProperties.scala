/*
 * Cathode [tests]
 */

package cathode
package data

import cats._
import cats.data._

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Shapeless._

object DummyAlgebra {

  sealed trait Op[A]
  case object Increment extends Op[Unit]
  case object Decrement extends Op[Unit]
  case object Read extends Op[Long]

  case class Env private (
    runLog: List[Op[_]] = Nil,
    value: Long = 0)

  object Env {
    val interpreter: Op ~> State[Env, ?] = λ[Op ~> State[Env, ?]]{
      case Increment ⇒ State(state ⇒ (state.copy(
        runLog = Increment :: state.runLog, value = state.value + 1), ()))
      case Decrement ⇒ State(state ⇒ (state.copy(
        runLog = Decrement :: state.runLog, value = state.value - 1), ()))
      case Read ⇒ State(state ⇒ (state.copy(
        runLog = Read :: state.runLog), state.value))
    }
  }

}

class StatefulInterpreterProperties extends Properties("StatefulInterpreter") {
  import DummyAlgebra._

  property("consistent state evaluation") = forAll { (ops: List[Op[_]]) ⇒
    val stateful = new StatefulInterpreter(Env.interpreter, Env())

    val zero = State.pure[Env, Any](())
    val (state0, res0) = ops.foldLeft(zero)((acc, op) ⇒
      acc.flatMap(_ ⇒ Env.interpreter(op).map(res ⇒ res: Any)))
      .run(Env()).value

    val res1 = ops.foldLeft((): Any)((acc, op) ⇒
      stateful(op))

    (res0 ?= res1) && (state0 ?= stateful.state)
  }

}
