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

import asmble.compile.jvm.MemoryBuffer
import asmble.run.jvm.Module.Compiled
import asmble.run.jvm.ScriptContext
import cats.Monad
import cats.data.EitherT
import cats.effect.LiftIO
import fluence.vm.VmError.WasmVmError.{ApplyError, GetVmStateError, InvokeError}
import fluence.vm.VmError.{InitializationError, NoSuchFnError, VmMemoryError}
import fluence.vm.wasm.{MemoryHasher, WasmFunction, WasmModuleMemory, module}

import scala.language.higherKinds
import scala.util.Try

/**
 * Wrapper of Wasm Module instance compiled by Asmble to Java class. Provides all functionality of Wasm modules
 * according to the Fluence protocol (invoke, parameter passing, hash computing). TODO: after removing alloc/
 * dealloc should be refactored to two modules types (extends the same trait): "master" (that has invoke method
 * and can routes call from user to "slaves") and "slave" (without invoke method that does only computation).
 *
 * @param name an optional module name (according to Wasm specification module name can be empty string (that is also
 *             "valid UTF-8") or even absent)
 * @param wasmMemory the memory of this module (please see comment in apply method to understand why it's optional now)
 * @param moduleInstance a instance of Wasm Module compiled by Asmble
 * @param allocateFunction a function used for allocation of a memory region for parameter passing
 * @param deallocateFunction a function used for deallocation of a memory region previously allocated
 *                          by allocateFunction
 * @param invokeFunction a function that represents main handler of Wasm module
 */
class EnvModule(
  override val name: Option[String],
  override val wasmMemory: WasmModuleMemory,
  override val moduleInstance: Any,
  private val spentGasFunction: WasmFunction,
  private val setSpentGasFunction: WasmFunction
) extends WasmModule(name, wasmMemory, moduleInstance) {

  /**
   * Allocates a memory region in Wasm module of supplied size by allocateFunction.
   *
   * @param size a size of memory that need to be allocated
   */
  def getSpentGas[F[_]: LiftIO: Monad](): EitherT[F, InvokeError, Int] =
    invokeWasmFunction(spentGasFunction, Nil)

  /**
   * Deallocates a previously allocated memory region in Wasm module by deallocateFunction.
   *
   * @param offset an address of the memory region to deallocate
   * @param size a size of memory region to deallocate
   */
  def clearSpentGas[F[_]: LiftIO: Monad](): EitherT[F, InvokeError, Unit] =
    invokeWasmFunction(setSpentGasFunction, Int.box(0) :: Nil)
      .map(_ ⇒ ())

}

object EnvModule {

  /**
    * Creates instance for specified module.
    *
    * @param moduleDescription a Asmble description of the module
    * @param scriptContext a Asmble context for the module operation
    * @param memoryHasher a hasher used for compute hash if memory
    */
  def apply[F[_]: Monad](
   moduleDescription: Compiled,
   scriptContext: ScriptContext,
   memoryHasher: MemoryHasher.Builder[F],
   spentGasFunction: WasmFunction,
   setSpentGasFunction: WasmFunction
  ): EitherT[F, ApplyError, module.WasmModule] =
    for {
      wasmModule <- WasmModule(moduleDescription, scriptContext, memoryHasher)



    } yield
      new module.WasmModule(
        Option(moduleDescription.getName),
        moduleMemory,
        moduleInstance
      )

}
