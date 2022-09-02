(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 Origin Labs                                   *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module Collections : sig
  module StringMap : Map.S with type key = string
  module IntMap : Map.S with type key = int
end

module Log : sig

  val feedback : ('a, Format.formatter, unit) format -> 'a
  val debug : ('a, Format.formatter, unit) format -> 'a

end

val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
val invalid_argument : ('a, Format.formatter, unit, 'b) format4 -> 'a

val numbits : int -> int
