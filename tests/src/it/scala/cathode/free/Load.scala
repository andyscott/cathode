/*
 * Cathode [tests]
 */

package cathode
package free

import io.gatling.core.Predef._
import io.gatling.core.action._
import io.gatling.core.action.builder._
import io.gatling.core.structure.ScenarioContext
import io.gatling.core.stats._
import io.gatling.core.stats.message.ResponseTimings
import io.gatling.commons.stats.Status

import scala.concurrent.duration._


import cats._
import cats.free._

class FooBarOps[F[_]](implicit I: Inject[FooBarOp, F]) {
  import FooBarOp._

  def foo(value: Any): Free[F, String] = Free.inject(Foo(value))
  def bar(value: Any): Free[F, Option[Any]] = Free.inject(Bar(value))
}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Random
//import scala.math.BigDecimal
//import scala.concurrent.blocking

sealed trait FooBarOp[A]
object FooBarOp {
  case class Foo(value: Any) extends FooBarOp[String]
  case class Bar(value: Any) extends FooBarOp[Option[Any]]



  def interpreter(implicit ec: ExecutionContext): FooBarOp ~> Future =

    λ[FooBarOp ~> Future] {
      case Foo(value) => Future {
        /*
        Thread.sleep(18)
        def loop(i: Int, r: BigDecimal): BigDecimal =
          if (i == 0) r
          else loop(i - 1,
            List.fill(100)(BigDecimal(Math.sqrt(Random.nextDouble))).reduce(_ + _) * r)
        val res = loop(30, BigDecimal(1))
         */
        Thread.sleep(22)
        val res = value
        s"$value ${res}"
      }
      case Bar(value) => Future {
        Thread.sleep(22)
        if (Random.nextBoolean) Some(value) else None
      }
    }
}



class FreeLoadSimulation extends Simulation {

  import akka.actor.{ Status => _, _}
  import scala.concurrent.Await

  type Program[A] = FooBarOp[A]
  val fooBar = new FooBarOps[Program]

  val program = for {
    res0 <- fooBar.foo("LOL")
    res1 <- fooBar.bar(res0).map(_ getOrElse "qq")
    res2 <- fooBar.foo(res1)
    res3 <- fooBar.bar(res2).map(_ getOrElse "!!")
  } yield res3

  val cathodeSystem = ActorSystem("cathode")
  val interpreter =
    FooBarOp.interpreter(_root_.monix.execution.Scheduler.io())


  class FreeLoadAction(val next: Action, statsEngine: StatsEngine, run: Free[Program, ?] ~> Future) extends ChainableAction  {
    override val name: String = "FreeLoadAction"
    def execute(session: Session): Unit = {
      val start = System.currentTimeMillis()
      val res0 = run(program)
      val res1 = Await.result(res0, Duration.Inf)
      val end = System.currentTimeMillis()

      val timings = ResponseTimings(start, end)
      statsEngine.logResponse(session, "free load", timings, Status("OK"), None, None)
      next ! session
    }
  }


  val mine = new ActionBuilder {
    def build(ctx: ScenarioContext, next: Action): Action = {
      val run = λ[Free[Program, ?] ~> Future](p =>
        FreeRun[Program](cathodeSystem, 100.seconds, p, interpreter))

      //import cats.instances.future._
      //import ExecutionContext.Implicits.global
      //val run = λ[Free[Program, ?] ~> Future](_.foldMap(interpreter))

      new FreeLoadAction(next, ctx.coreComponents.statsEngine, run)
    }
  }

  val scn = scenario("freedom isn't free")
    .exec(mine)

  setUp(
    scn.inject(
      constantUsersPerSec(500) during 5.seconds,
      rampUsers(10000) over 10.seconds
    )
  )

}
