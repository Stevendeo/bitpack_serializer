(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Types of binary buffer *)
type writer
type reader

(** 1. Writer *)

(** Initializes the buffer as a writer.
    If `with_dict` is set to true (by default `true`), strings will be
    registered in a dictionary (useful when there are multiple times the same
    string. *)
val initialize_writer : ?with_dict:bool -> unit -> writer

(** Returns the bytes representation of the buffer *)
val finalize : writer -> Bytes.t

(** `write buff v size` writes v as a 'size' bit value. Value must be positive. *)
val write : writer -> int64 -> int -> unit

(** Writes a boolean on 1 bit *)
val write_bool : writer -> bool -> unit

(** Write unsigned ints *)
val write_uint8 : writer -> int -> unit
val write_uint16 : writer -> int -> unit
val write_uint32 : writer -> int -> unit
val write_uint63 : writer -> int64 -> unit

(** Writes on the buffer an unsigned int prefixed by its size. *)
val write_signed_int : writer -> int -> unit

(** Writes an unbounded integer *)
val write_z : writer -> Z.t -> unit

(** Writes bytes *)
val write_bytes : writer -> Bytes.t -> unit

(** Writes a string *)
val write_string : writer -> string -> unit

(** Write a string representation *)
val write_str_repr : writer -> string -> unit

(** 2. Reader *)

(** Initializes the buffer as a reader *)
val initialize_reader : ?with_dict:bool -> Bytes.t -> reader

(** read buff n reads n bits on the buffer as an unsigned value *)
val read : reader -> int -> int64

(** Reads 1 bit, returns true if 1, 0 otherwise *)
val read_bool : reader -> bool

(** Reads unsigned integers *)
val read_uint8 : reader -> int
val read_uint16 : reader -> int
val read_uint32 : reader -> int
val read_uint63 : reader -> int64

(** Reads an unsigned int prefixed by its size. *)
val read_signed_int : reader -> int

(** Reads an unbounded integer *)
val read_z : reader -> Z.t

(** Reads bytes *)
val read_bytes : reader -> Bytes.t

(** Reads a string *)
val read_string : reader -> string

(** Reads a string representation *)
val read_str_repr : reader -> string


val print_stats : Format.formatter -> writer -> unit

(** Statistics *)

val stats_add_i_custom : writer -> string -> int -> unit
