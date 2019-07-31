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
 * @param allocateFunction a function used for allocation of a memory region for parameter passing
 * @param deallocateFunction a function used for deallocation of a memory region previously allocated
 *                          by allocateFunction
 * @param invokeFunction a function that represents main handler of Wasm module
 */
class MainModule(
  private val module: WasmModule,
  private val allocateFunction: WasmFunction,
  private val deallocateFunction: WasmFunction,
  private val invokeFunction: WasmFunction
) {

  /**
   * Allocates a memory region in Wasm module of supplied size by allocateFunction.
   *
   * @param size a size of memory that need to be allocated
   */
  def allocate[F[_]: LiftIO: Monad](size: Int): EitherT[F, InvokeError, Int] =
    module.invokeWasmFunction(allocateFunction, Int.box(size) :: Nil)

  /**
   * Deallocates a previously allocated memory region in Wasm module by deallocateFunction.
   *
   * @param offset an address of the memory region to deallocate
   * @param size a size of memory region to deallocate
   */
  def deallocate[F[_]: LiftIO: Monad](offset: Int, size: Int): EitherT[F, InvokeError, Unit] =
    module.invokeWasmFunction(deallocateFunction, Int.box(offset) :: Int.box(size) :: Nil)
      .map(_ ⇒ ())

  /**
   * Invokes invokeFunction which exported from Wasm module with provided arguments.
   *
   * @param args arguments for invokeFunction
   */
  def invoke[F[_]: LiftIO: Monad](args: List[AnyRef]): EitherT[F, InvokeError, Int] =
    module.invokeWasmFunction(invokeFunction, args)

  /**
   * Reads [offset, offset+size) region from the module memory.
   *
   * @param offset an offset from which read should be started
   *  @param size bytes count to read
   */
  def readMemory[F[_]: Monad](offset: Int, size: Int): EitherT[F, VmMemoryError, Array[Byte]] =
    module.wasmMemory.readBytes(offset, size)

  /**
   * Writes array of bytes to module memory.
   *
   * @param offset an offset from which write should be started
   * @param injectedArray an array that should be written into the module memory
   */
  def writeMemory[F[_]: Monad](offset: Int, injectedArray: Array[Byte]): EitherT[F, VmMemoryError, Unit] =
    module.wasmMemory.writeBytes(offset, injectedArray)

  override def toString: String = module.name.getOrElse("<no-name>")
}

object MainModule {

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
   allocationFunctionName: String,
   deallocationFunctionName: String,
   invokeFunctionName: String
  ): EitherT[F, ApplyError, MainModule] =
    for {
      module <- WasmModule(moduleDescription, scriptContext, memoryHasher)

      (allocMethod, deallocMethod, invokeMethod) = moduleDescription.getCls.getDeclaredMethods.toStream
        .filter(method ⇒ Modifier.isPublic(method.getModifiers))
        .map(method ⇒ WasmFunction(method.getName, method))
        .foldLeft((Option.empty[WasmFunction], Option.empty[WasmFunction], Option.empty[WasmFunction])) {
          case (acc @ (None, _, _), m @ WasmFunction(`allocationFunctionName`, _)) ⇒
            acc.copy(_1 = Some(m))

          case (acc @ (_, None, _), m @ WasmFunction(`deallocationFunctionName`, _)) ⇒
            acc.copy(_2 = Some(m))

          case (acc @ (_, _, None), m @ WasmFunction(`invokeFunctionName`, _)) ⇒
            acc.copy(_3 = Some(m))

          case (acc, _) ⇒ acc
        }

    } yield
      new MainModule(
        module,
        allocMethod.get,
        deallocMethod.get,
        invokeMethod.get
      )

}
