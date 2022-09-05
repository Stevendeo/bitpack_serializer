(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 OcamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Lenses are useful tools defining data-types encodings. Lenses provides
    in a single OCaml value equivalent serializes and deserializers. *)

(** {1 Lenses} *)

(** The type of lenses. An ['a lens] will provide symetical functions to
    read and write values of type ['a]. *)
type 'a t

(** Writes in a buffer. *)
val write : 'a t -> Buffer.writer -> 'a -> unit

(** Reads from a buffer. *)
val read : 'a t -> Buffer.reader -> 'a

(** Basic lenses *)

(** A lens for unsigned int of fixed size. *)
val uint : size:int -> int64 t

(** A lens for signed integers. *)
val sint : int t

(** A lens for Zarith integers *)
val zint : Z.t t

(** A lens for strings. *)
val string : string t

(** A lens for unknown sized bytes. *)
val bytes : bytes t

(** A lens for fixed sized bytes. *)
val fixed_size_bytes : num_bytes:int -> bytes t

(** Given two lenses for two types, creates a lens for a pair of these types. *)
val conj : 'a t -> 'b t -> ('a * 'b) t

(** For creating a lens for disjunctions, we define the ['a case] type
    for the {!Lens.disj} function to build new lenses. *)
type 'a case

(** Builds a case for disjunctive lenses. *)
val case :
  destruct:('a -> 'b option) ->
  construct:('b -> 'a) ->
  'b t ->
  'a case

(** Creates a lens from an array of cases.
    Raises [Failure] when reading or writing if no case matches the encoding. *)
val disj : 'a case array -> 'a t

(** Builds a self dependent lens. *)
val mu : ('a t -> 'a t) -> 'a t
