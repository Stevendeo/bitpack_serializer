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

module Collections = struct
  module StringMap = Map.Make(String)
  module IntMap = Map.Make(Int)
end

module Log = struct

  let varname = "BINARY_ENCODING"

  let debug_level =
    match int_of_string @@ Sys.getenv varname with
    | exception Not_found -> 0
    | exception (Failure _) ->
        failwith @@
        Format.sprintf
          "Bad value of environment variable %s" varname
    | i -> i

  let feedback m =
    if debug_level >= 1
    then Format.printf m
    else Format.ifprintf Format.std_formatter m

  let debug m =
    if debug_level >= 2
    then Format.printf m
    else Format.ifprintf Format.std_formatter m
end

let failwith m = Format.kasprintf failwith m

let numbits (i : int) : int =
  i |> Z.of_int |> Z.numbits
