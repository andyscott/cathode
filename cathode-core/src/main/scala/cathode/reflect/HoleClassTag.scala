/*
 * Cathode [cathode-core]
 */

package cathode
package reflect

import scala.reflect.ClassTag

/** Type class encoding the ability to get the class tag for a single
  * hole.
  */
trait HoleClassTag[F[_]] {
  def classTagA[A](fa: F[A]): ClassTag[A]
}
