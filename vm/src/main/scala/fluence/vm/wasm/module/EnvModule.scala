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

package fluence.vm.wasm.module

import java.lang.reflect.Modifier

import asmble.run.jvm.Module.Native
import asmble.run.jvm.ScriptContext
import cats.Monad
import cats.data.EitherT
import cats.effect.LiftIO
import fluence.vm.VmError.WasmVmError.{ApplyError, InvokeError}
import fluence.vm.VmError.{InitializationError, NoSuchFnError}
import fluence.vm.utils.safelyRunThrowable
import fluence.vm.wasm._

import scala.language.higherKinds

/**
 * Represent the state of the environment module.
 */
final case class EnvState(spentGas: Long, EIC: Long)

/**
 * Wrapper for Environment module registered by Asmble (please find more info in WasmModule docs).
 * This module could be used for gas metering.
 *
 * @param instance a instance of Wasm Module compiled by Asmble
 * @param getSpentGasFunction a function returns a spent gas count
 * @param getEICFunction a function returns count of executed instructions
 * @param clearStateFunction a function sets spent gas count
 */
class EnvModule(
  private val instance: ModuleInstance,
  private val getSpentGasFunction: WasmFunction,
  private val getEICFunction: WasmFunction,
  private val clearStateFunction: WasmFunction
) {

  /**
   * Returns spent gas by Wasm code.
   */
  def getState[F[_]: LiftIO: Monad](): EitherT[F, InvokeError, EnvState] =
    for {
      // TODO : in future it needs to be refactored to return EnvState as a entire struct from Asmble
      //  by only one function call

      // it is known that functions for getting spent gas and eic should return value of Integer type
      // and then this value is used only like a number and it is pretty safe to use fold here
      spentGas ← getSpentGasFunction(instance, Nil).map(_.fold(0L)(_.longValue()))
      eic ← getEICFunction(instance, Nil).map(_.fold(0L)(_.longValue()))
    } yield EnvState(spentGas, eic)

  /**
   * Clears the spent gas count.
   */
  def clearState[F[_]: LiftIO: Monad](): EitherT[F, InvokeError, Unit] =
    clearStateFunction(instance, Nil).map(_ ⇒ ())

}

object EnvModule {

  /**
   * Creates instance for specified module.
   *
   * @param moduleDescription a Asmble description of the module
   * @param scriptContext a Asmble context for the module operation
   * @param getSpentGasFunctionName a name of the function returns a spent gas count
   * @param getEICFunctionName a name of the function returns a spent gas count
   * @param clearStateFunctionName a name of the function sets a spent gas count
   */
  def apply[F[_]: Monad](
    moduleDescription: Native,
    scriptContext: ScriptContext,
    getSpentGasFunctionName: String,
    getEICFunctionName: String,
    clearStateFunctionName: String
  ): EitherT[F, ApplyError, EnvModule] =
    for {

      moduleInstance ← safelyRunThrowable(
        moduleDescription.instance(scriptContext),
        e ⇒ InitializationError(s"Unable to initialize the environment module", Some(e))
      )

      moduleMethods: Stream[WasmFunction] = moduleDescription.getCls.getDeclaredMethods.toStream
        .filter(method ⇒ Modifier.isPublic(method.getModifiers))
        .map(method ⇒ WasmFunction(method.getName, method))

      (getSpentGas: WasmFunction, getEIC: WasmFunction, clearState: WasmFunction) ← EitherT.fromOption(
        moduleMethods
          .scanLeft((Option.empty[WasmFunction], Option.empty[WasmFunction], Option.empty[WasmFunction])) {
            case (acc, m @ WasmFunction(`getSpentGasFunctionName`, _)) ⇒
              acc.copy(_1 = Some(m))
            case (acc, m @ WasmFunction(`getEICFunctionName`, _)) ⇒
              acc.copy(_2 = Some(m))
            case (acc, m @ WasmFunction(`clearStateFunctionName`, _)) ⇒
              acc.copy(_3 = Some(m))
            case (acc, _) ⇒
              acc
          }
          .collectFirst {
            case (Some(m1), Some(m2), Some(m3)) ⇒ (m1, m2, m3)
          },
        NoSuchFnError(
          s"The env module must have function with names $getSpentGasFunctionName, $getEICFunctionName, $clearStateFunctionName"
        ): ApplyError
      )

    } yield
      new EnvModule(
        ModuleInstance(moduleInstance),
        getSpentGas,
        getEIC,
        clearState
      )

}
