/*
 * Cathode [cathode-core]
 */

package cathode
package data

import cats._
import cats.data._

/** Convert a `State` interpreter for the `F` algebra into an interpreter
  * directly to the `Applicative` functor `G` by maintaining a mutable
  * reference to an internal environment `S`.
  *
  * The intended use case is for using `State` based mocks during
  * development/demos of production applications.
  *
  * This is thread safe as updates to the environment ref are
  * synchronized.
  *
  * @param interp the interpreter to `State`
  * @param state0 the initial environment
  */
class StatefulInterpreter[F[_], G[_], S <: AnyRef] private[data] (
    interp: F ~> State[S, ?], state0: S
)(implicit G: Applicative[G]) extends (F ~> G) {
  @volatile private[data] var state: S = state0
  override def apply[A](fa: F[A]): G[A] = state.synchronized {
    val (nextState, res) = interp(fa).run(state).value
    state = nextState
    G.pure(res)
  }
}

object StatefulInterpreter {

  def apply[F[_], G[_]: Applicative, S <: AnyRef](
    interp: F ~> State[S, ?], state0: S): StatefulInterpreter[F, G, S] =
    new StatefulInterpreter(interp, state0)

  def apply[G[_]]: PartiallyAppliedApply[G] =
    new PartiallyAppliedApply[G]

  final class PartiallyAppliedApply[G[_]] private[StatefulInterpreter] {
    def apply[F[_], S <: AnyRef](interp: F ~> State[S, ?], state0: S)(implicit G: Applicative[G]): StatefulInterpreter[F, G, S] =
      new StatefulInterpreter(interp, state0)
  }
}
