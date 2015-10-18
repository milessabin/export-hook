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

package object export {
  type ExportHighPriority[+T] = Export0[T]
  val ExportHighPriority = Export0

  type ExportOptional[+T] = Export1[T]
  val ExportOptional = Export1

  type ExportSubclass[+T] = Export2[T]
  val ExportSubclass = Export2

  type ExportAlgebraic[+T] = Export3[T]
  val ExportAlgebraic = Export3

  type ExportInstantiated[+T] = Export4[T]
  val ExportInstantiated = Export4

  type ExportGeneric[+T] = Export5[T]
  val ExportGeneric = Export5

  type ExportDefault[+T] = Export6[T]
  val ExportDefault = Export6

  type ExportLowPriority[+T] = Export7[T]
  val ExportLowPriority = Export7

  type Export[+T] = ExportGeneric[T]
  val Export = ExportGeneric
}
