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

(** The Buffer defines the methods for reading and writing in a
    buffer. The compression algorithm used by the library works by writing
    bits after bits instead of bytes. *)

(** A dictionary type.
    It is used as an argument for initialzing writers and readers.
*)
type 'dict dictionary =
  | NoDictionary
  | Dictionary of 'dict

(** {1 Writer} *)

(** A writer is a {{: https://v2.ocaml.org/api/Bytes.html }Bytes} buffer where is
    written the data. It also contains an optional dictionary for string
    storage.
    It also holds statistics on the repartition of bits for each datatype
    (unsigned int, signed int and string). *)
type writer

(** Initializes a new buffer as a writer.
    If [with_dict] is set to [NoDictionary], strings will be written in the
    buffer.
    If [with_dict] is set to [Dictionary None] (by default), strings will be
    replaced by an unknown size integers; a disctionary will be saved and
    concatenated at the beginning of the buffer after finalization.
    If [with_dict] is set to [Dictionary (Some size)], it does the same than
    [Dictionary None] but it is assumed there will be no more than [size] words
    to save.
    registered in a dictionary (useful when the same string apprears at
    different places) and replaced by integers in the buffer.
    Variable [init_size] is the initial size of the writing bytes buffer
    (by default: 4096)
*)
val initialize_writer :
  ?with_dict:int option dictionary ->
  ?init_size:int ->
  unit -> writer

(** Returns the bytes representation of the buffer. If there is a dictionary,
    it is written at the beginning of the buffer.
*)
val finalize : writer -> Bytes.t

(** [write buff v size]

    Writes [v] on [buff] using [size] bits. Value must be positive.
    Fails with [Invalid_argument] if [size] is not in [[0, 63]] or if
    [v] is negative or greater than [2^size].
*)
val write : writer -> int64 -> int -> unit

(** Writes a boolean on 1 bit: [0] if [false], [1] if [true]. *)
val write_bool : writer -> bool -> unit

(** Write unsigned ints *)

(** Equivalent to [write w v 8] *)
val write_uint8 : writer -> int -> unit

(** Equivalent to [write w v 16] *)
val write_uint16 : writer -> int -> unit

(** Equivalent to [write w v 32] *)
val write_uint32 : writer -> int -> unit

(** Equivalent to [write w v 63] *)
val write_uint63 : writer -> int64 -> unit

(** Writes an unbounded integer.*)
val write_z : writer -> Z.t -> unit

(** Same as [write_z w (Z.of_int i)] *)
val write_signed_int : writer -> int -> unit

(** [write_bytes_known_size ~len w b]
    Writes the first [len] bytes of [b] in argument.
*)
val write_bytes_known_length : len:int -> writer -> Bytes.t -> unit

(** Writes a bytes buffer of any size. *)
val write_bytes : writer -> Bytes.t -> unit

(** Write a string representation. If the buffer is associated to a dictionary,
    writes the its associated integer instead. *)
val write_str_repr : writer -> string -> unit

(** {1 Reader} *)

(** A reader buffer. *)
type reader

(** Initializes a buffer as a reader.
    The [with_dict] argument must be the same as the argument used for
    initializing the writer.
*)
val initialize_reader :
  ?with_dict:int option dictionary ->
  Bytes.t -> reader

(** read buff n reads n bits on the buffer as an unsigned value *)
val read : reader -> int -> int64

(** Reads 1 bit, returns [true] if [1], [false] otherwise *)
val read_bool : reader -> bool

(** Reads unsigned integers *)

(** Reads 8 bits *)
val read_uint8 : reader -> int

(** Reads 16 bits *)
val read_uint16 : reader -> int

(** Reads 32 bits *)
val read_uint32 : reader -> int

(** Reads 63 bits *)
val read_uint63 : reader -> int64

(** Reads an unsigned int prefixed by its size. *)
val read_signed_int : reader -> int

(** Reads an unbounded integer *)
val read_z : reader -> Z.t

(** Reads [len] bytes and returns it as a bytes buffer. *)
val read_bytes_known_length : len:int -> reader -> bytes

(** Reads a integer representing the number of bytes to read,
    then reads and return these bytes. *)
val read_bytes : reader -> Bytes.t

(** Reads a string representation *)
val read_str_repr : reader -> string

(** {1 Statistics} *)

(** Prints statistics of bits repartition of a buffer. *)
val print_stats : Format.formatter -> writer -> unit
