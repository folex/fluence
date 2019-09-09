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

package fluence.morph.http4s

import cats.data.EitherT
import cats.syntax.functor._
import cats.effect.Sync
import fluence.morph.=?>
import org.http4s.{EntityDecoder, InvalidMessageBodyFailure, MediaRange}

import scala.language.higherKinds

object Http4sMorph {

  def decodeStringEntity[F[_]: Sync, T](
    mediaRange: MediaRange = MediaRange.`*/*`
  )(implicit dec: String =?> T): EntityDecoder[F, T] =
    EntityDecoder.decodeBy[F, T](mediaRange)(
      msg ⇒
        EitherT(EntityDecoder.decodeString(msg).map(dec.run(_)))
          .leftMap(e ⇒ InvalidMessageBodyFailure(e.getMessage, e.cause))
    )
}
