package fluence.morph.circe

import cats.data.Kleisli
import fluence.morph.{=?>, MorphError}
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import cats.syntax.either._

object CirceMorph {

  implicit def morphFromEncoder[T: Encoder]: T =?> Json =
    Kleisli(t ⇒ Encoder[T].apply(t).asRight)

  implicit def morphFromDecoder[T: Decoder]: Json =?> T =
    Kleisli(j ⇒ Decoder[T].decodeJson(j).fold(f ⇒ MorphError("Cannot decode JSON", Some(f)).asLeft, _.asRight))

  implicit def morphToDecoder[T](implicit m: Json =?> T): Decoder[T] =
    (c: HCursor) => m.run(c.value).fold(
      e ⇒ e.cause match {
        case Some(f: DecodingFailure) ⇒ f.asLeft
        case _ ⇒ DecodingFailure(e.getMessage, Nil).asLeft
      },
      _.asRight
    )

  implicit val jsonToNoSpaces: Json =?> String =
    Kleisli(j ⇒ j.noSpaces.asRight)
}
