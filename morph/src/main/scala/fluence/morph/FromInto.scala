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

import cats.arrow.Category
import cats.instances.either._

case class FromInto[A, B](into: A =?> B, from: B =?> A)

object FromInto {
  implicit def fromInto[A, B](implicit ab: A =?> B, ba: B =?> A): FromInto[A, B] =
    FromInto(ab, ba)

  implicit def swapFromInto[A, B](implicit ab: A <=?> B): B <=?> A =
    FromInto(ab.from, ab.into)

  implicit def pickMorphInto[A, B](implicit ab: A <=?> B): A =?> B = ab.into

  implicit def pickMorphFrom[A, B](implicit ab: A <=?> B): A From B = ab.from

  implicit object FromIntoCategory extends Category[<=?>] {
    override def id[A]: <=?>[A, A] =
      FromInto(identityMorph, identityMorph)

    override def compose[A, B, C](f: <=?>[B, C], g: <=?>[A, B]): <=?>[A, C] =
      FromInto(f.into compose g.into, f.from andThen g.from)
  }
}
