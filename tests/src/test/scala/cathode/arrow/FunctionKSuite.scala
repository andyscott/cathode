/*
 * Cathode [tests]
 */

package cathode
package arrow

import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.prop._

import org.scalacheck._
import akka.testkit._

import cats.~>
import cats.instances.future._
import akka.actor._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._

import cathode.util.FooADT

class FunctionKSuite extends TestKit(ActorSystem())
    with FlatSpecLike
    with Matchers
    with ScalaFutures
    with GeneratorDrivenPropertyChecks
    with IntegrationPatience {

  import system.dispatcher

  val timeout = FiniteDuration(
    patienceConfig.timeout.length, patienceConfig.timeout.unit)

  def through[F[_]](f: F ~> Future)(implicit ec: ExecutionContext): F ~> Future = {
    val ref = system.actorOf(Props(new FunctionKActor(f)))
    AskFunctionK(ref, timeout)
  }

  def roundtripConsistency[F[_]](f: F ~> Future)(implicit arbFA: Arbitrary[F[_]]) =
    forAll { (value: F[_]) ⇒
      f(value).futureValue should be(through(f).apply(value).futureValue)
    }

  "round tripping an ADT ~> Future through Akka" should "behave the same as that ADT ~> Future" in {
    import FooADT._
    roundtripConsistency[Foo](Foo.interp[Future])
  }

}
