(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 - 2026 OcamlPro                                    *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Bitpack_serializer
open Buffer

let id = "test-string-list"

type t = string list

let list = [
  "Alice";
  "Bob";
  "Charlie";
  "Alice";
  "David";
  "Bob";
  "Eve";
  "Alice";
  "Frank";
  "Charlie";
  "Bob";
  "Grace";
  "Heidi";
  "Eve";
  "Alice";
  "Ivan";
  "Bob";
  "Judy";
  "Charlie";
  "Alice";
]

let lens =
  Lens.mu (fun self ->
    let writer b = function
      | [] -> write_bool b false
      | hd :: tl ->
         write_bool b true;
         write_str_repr b hd;
         Lens.write self b tl
    and reader b =
      match read_bool b with
      | false -> []
      | true ->
         let v = read_str_repr b in
         v :: Lens.read self b
    in
    Lens.make ~writer ~reader)

let to_test =
  [
    list, Lib.{lens; dictionary = `NoDictionary};
    list, Lib.{lens; dictionary = `Dictionary None};
    list, Lib.{lens; dictionary = `Dictionary (Some 15)};
  ]

let pp fmt = Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";") Format.pp_print_string)

let eq = List.equal String.equal

