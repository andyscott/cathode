/*
 * Cathode [cathode-core]
 */

package object cathode {
  private[this] def unexpected: Nothing = sys.error("Unexpected invocation")

  implicit def neq[A, B]: A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A]: A =:!= A = unexpected
  implicit def neqAmbig2[A]: A =:!= A = unexpected
}

package cathode {
  trait =:!=[A, B] extends Serializable
}
