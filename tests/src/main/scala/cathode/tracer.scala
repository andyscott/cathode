/*
 * Cathode [tests]
 */

package cathode
package example

import cats._
import cats.syntax.all._

object FooAlgebra {
  sealed trait Op[A]
  case object Foo extends Op[String]

  case class FooOps[F[_]](lift: Op ~> F = interpreter) {
    def foo(): F[String] = lift(Foo)
  }

  val interpreter: Op ~> Id = λ[Op ~> Id] {
    case Foo ⇒ "Foo"
  }
}

object BarAlgebra {
  sealed trait Op[A]
  case object Bar extends Op[String]

  case class BarOps[F[_]](lift: Op ~> F = interpreter) {
    def bar(): F[String] = lift(Bar)
  }

  val interpreter: Op ~> Id = λ[Op ~> Id] {
    case Bar ⇒ "Bar"
  }
}

object BazAlgebra {
  sealed trait Op[A]
  case object Baz extends Op[String]

  case class BazOps[F[_]](lift: Op ~> F) {
    def baz(): F[String] = lift(Baz)
  }

  class BazInterpreter[F[_]: Monad](
      fooOps: FooAlgebra.FooOps[F],
      barOps: BarAlgebra.BarOps[F]
  ) extends (Op ~> F) {
    override def apply[A](op: Op[A]): F[A] = op match {
      case Baz ⇒ for {
        foo ← fooOps.foo()
        bar ← barOps.bar()
      } yield s"$foo$bar"
    }
  }

}

object TracerApp {

  def main(args: Array[String]): Unit = {

    val fooOps0 = FooAlgebra.FooOps()
    val barOps0 = BarAlgebra.BarOps()
    val bazOps0 = BazAlgebra.BazOps(
      new BazAlgebra.BazInterpreter(fooOps0, barOps0))

    val res0 = bazOps0.baz()
    println(s"baz0: $res0")

    val fooOps1 = FooAlgebra.FooOps()
    val barOps1 = BarAlgebra.BarOps()

    case class Aug[F[_], A](op: F[A], id: String)

    val augment: BazAlgebra.Op ~> Aug[BazAlgebra.Op, ?] =
      new (BazAlgebra.Op ~> Aug[BazAlgebra.Op, ?]) {
        override def apply[A](op: BazAlgebra.Op[A]): Aug[BazAlgebra.Op, A] = {
          Aug(op, s"cid_${System.currentTimeMillis}")
        }
      }

    def unwrap(interp: BazAlgebra.Op ~> Id): Aug[BazAlgebra.Op, ?] ~> Id = new (Aug[BazAlgebra.Op, ?] ~> Id) {
      override def apply[A](aug: Aug[BazAlgebra.Op, A]): Id[A] = {
        println(s">> ${aug.id} ${aug.op}")
        interp(aug.op)
      }
    }

    val zzz: Aug[BazAlgebra.Op, ?] ~> Id = unwrap(new BazAlgebra.BazInterpreter(fooOps1, barOps1))
    val bazInterpreter: BazAlgebra.Op ~> Id =
      augment andThen zzz
    val bazOps1 = BazAlgebra.BazOps(bazInterpreter)

    val res1 = bazOps1.baz()
    println(s"baz1: $res1")

  }

}
