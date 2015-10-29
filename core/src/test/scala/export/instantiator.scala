/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package instantiator

import export._
import tca._, tca1._

@reexports[InstantiatedTcA]
object tc1instantiator

trait InstantiatedTcA[T] extends TcA[T]

@exports(Instantiated)
object InstantiatedTcA {
  implicit def instantiate[F[_], T](implicit tc1f: TcA1[F]): InstantiatedTcA[F[T]] =
    new InstantiatedTcA[F[T]] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (instantiate)" else {
          val seen1 = seen+this
          s"Instantiate ${tc1f.safeDescribe(seen1)}"
        }
    }
}
