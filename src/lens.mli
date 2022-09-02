(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 Ocaml Pro                                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** The type of lenses *)
type 'a t


(** Writes in a buffer *)
val write : 'a t -> Buffer.writer -> 'a -> unit

(** Reads from a buffer *)
val read : 'a t -> Buffer.reader -> 'a

(** Basic lenses *)
val uint : size:int -> int64 t
val sint : int t
val zint : Z.t t
val string : string t
val conj : 'a t -> 'b t -> ('a * 'b) t

type 'a case

(** Builds a case for disjunctive lenses. *)
val case :
  ('a -> 'b option) ->
  ('b -> 'a) ->
  'b t ->
  'a case

(** Creates a lens from an array of cases. *)
val disj : 'a case array -> 'a t

val mu : ('a t -> 'a t) -> 'a t
