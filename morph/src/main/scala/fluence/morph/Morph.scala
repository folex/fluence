/*
 * Copyright 2018 Fluence Labs Limited
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

package fluence.morph

import cats.instances.either._

object Morph extends {

  def into[A, B](implicit ab: A =?> B): A =?> B = ab

  def from[A, B](implicit ba: A From B): B =?> A = ba

  def connect[A, B, C](implicit ab: A =?> B, bc: B =?> C): A =?> C = ab andThen bc
  def connect[A, B, C, D](implicit ab: A =?> B, bc: B =?> C, cd: C =?> D): A =?> D = ab andThen bc andThen cd

}
