/*
 * Cathode [tests]
 */

package cathode
package util

import cats._

import org.scalacheck._

object FooADT {
  import Arbitrary._

  sealed trait Foo[A]
  case class StringFoo(v: String) extends Foo[String]
  case class IntFoo(v: Int) extends Foo[Int]
  case class BooleanFoo(v: Boolean) extends Foo[Boolean]

  implicit val arbFoo: Arbitrary[Foo[_]] =
    Arbitrary(Gen.oneOf(
      arbitrary[String].flatMap(StringFoo.apply),
      arbitrary[Int].flatMap(IntFoo.apply),
      arbitrary[Boolean].flatMap(BooleanFoo.apply)))

  object Foo {

    def interp[F[_]: Applicative] =
      λ[Foo ~> F] {
        case StringFoo(v)  ⇒ Applicative[F].pure(v)
        case IntFoo(v)     ⇒ Applicative[F].pure(v)
        case BooleanFoo(v) ⇒ Applicative[F].pure(v)
      }

  }
}
