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

package fluence.kad

import cats.kernel.laws.discipline.{MonoidTests, OrderTests}
import cats.tests.CatsSuite
import fluence.kad.protocol.Key
import org.scalacheck.{Arbitrary, Gen}

/**
 * Kademlia Keys form a Monoid with Order:
 * - Kademlia XOR distance acts as monoidal combine
 * - Empty key (filled with zeroes) is a neutral element
 * - All keys are ordered by Kademlia distance
 *
 * This spec just checks that all laws for Monoid and Order are met
 */
class KeyLawsSpec extends CatsSuite {

  private implicit val arbKey: Arbitrary[Key] =
    Arbitrary(Gen.listOfN[Byte](Key.Length, Arbitrary.arbByte.arbitrary).map(_.toArray).map(Key.fromBytes.unsafe))

  private implicit val arbKeyF: Arbitrary[Key ⇒ Key] =
    Arbitrary(Gen.const(k ⇒ Key.fromBytes.unsafe(k.bits.reverse.toByteArray)))

  checkAll("Key.monoid", MonoidTests[Key].monoid)
  checkAll("Key.order", OrderTests[Key].order)

}
