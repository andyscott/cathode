/*
 * Cathode [cathode-core]
 */

// As it stands, pretty much all of the following as been borrowed
// from Shapeless (to avoid adding another dependency)

package object cathode {
  private[this] def unexpected: Nothing = sys.error("Unexpected invocation")

  implicit def neq[A, B]: A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A]: A =:!= A = unexpected
  implicit def neqAmbig2[A]: A =:!= A = unexpected

  type @@[+T, U] = T with tag.Tagged[U]
}

package cathode {
  trait =:!=[A, B] extends Serializable

  object tag {
    def apply[U] = new Tagger[U]

    trait Tagged[U]

    class Tagger[U] {
      def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
    }
  }
}
