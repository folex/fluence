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

package fluence.morph.scodec

import cats.data.Kleisli
import fluence.morph.{<=?>, MorphError}
import scodec.bits.ByteVector

import scala.util.Try
import cats.syntax.either._

object ScodecMorph {

  object Base64 {
    implicit val byteVectorB64: String <=?> ByteVector =
      (
        Kleisli(s ⇒ ByteVector.fromBase64Descriptive(s).leftMap(MorphError(_))),
        Kleisli(bv ⇒ bv.toBase64.asRight[MorphError])
      )
  }

  object Base58 {
    implicit val byteVectorB58: String <=?> ByteVector =
      (
        Kleisli(s ⇒ ByteVector.fromBase58Descriptive(s).leftMap(MorphError(_))),
        Kleisli(bv ⇒ Right(bv.toBase58))
      )
  }

  object Hex {
    implicit val byteVectorHex: String <=?> ByteVector =
      (
        Kleisli(s ⇒ ByteVector.fromHexDescriptive(s).leftMap(MorphError(_))),
        Kleisli(bv ⇒ Right(bv.toHex))
      )
  }

  object RawString {
    implicit val byteVectorRawString: String <=?> ByteVector =
      (
        Kleisli(str ⇒ Right(ByteVector(str.getBytes))),
        Kleisli(
          bv ⇒
            Try(new String(bv.toArray)).toEither
              .leftMap(t ⇒ MorphError("Cannot convert byte vector to raw string", Some(t)))
        )
      )
  }

}
