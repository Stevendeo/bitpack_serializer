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

open Utils
open Collections

(** Keeps track of statistics on the buffer content *)
type stats = {
  mutable bits_string : int;
  mutable bits_int : int;
  mutable bits_z : int;
}

(** The type of binary buffers. 'offset' represents the current bit position. *)
type t =
  {
    mutable buffer : Bytes.t;
    mutable offset : int;
  }

type 'dict dictionary =
  | NoDictionary
  | Dictionary of 'dict

(** A reading specialized buffer. The dictionary is used to decode strings that
    have been encoded as integers. *)
type reader = {
  mutable reader : t;
  mutable dictionary : string IntMap.t dictionary
}

(** A writing specialized buffer. The dictionary is used to encode strings
    into integers. *)
type writer = {
  mutable writer : t;
  mutable dictionary : int StringMap.t ref dictionary;
  mutable stats : stats
}

let default_with_dict = true

(** Statistic utilities *)
let stats_add_i_bits_string t i = t.stats.bits_string <- t.stats.bits_string + i
let stats_add_i_bits_int t i = t.stats.bits_int <- t.stats.bits_int + i
let stats_del_i_bits_int t i = t.stats.bits_int <- t.stats.bits_int - i
let stats_add_i_bits_z t i = t.stats.bits_z <- t.stats.bits_z + i
let stats_del_i_bits_z t i = t.stats.bits_z <- t.stats.bits_z - i

(** Prints an int64 in binary, with at least 8 bits. *)
let print_int_bin fmt i =
  let open Int64 in
  let rec loop acc i =
    if Int64.(i = zero) && List.length acc >= 8 then acc
    else loop (logand i one :: acc) (shift_right_logical i 1)
  in
  let bini = loop [] i in
  Format.fprintf fmt "0b";
  List.iter
    (fun i -> Format.fprintf fmt "%Ld" i)
    bini

(** Writes '0' everywhere on the buffer. When writing, the buffer must be
    empty. *)
let clean_buffer size =
  let b = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.set_int8 b i 0
  done;
  b

(** Returns the byte position and the bit position on the said byte
    (example : offsets 15 = (1, 7), because the 15th bit is the 7th bit of the
    1st byte.) *)
let offsets (i : int) : int * int = i / 8, (i mod 8)

(** Same than before, but for the buffer *)
let buffer_offsets (t : t) : int * int = offsets t.offset

(** Retyrns the number of bytes needed to represent 'i' bits. *)
let bytes_needed (i : int) : int =
  if i mod 8 = 0 then i/8 else i/8 + 1

(** Returns the used buffer size. *)
let buffer_size (t : t) : int =
  bytes_needed t.offset

(** Prints the offset in argument *)
let pp_offset fmt i =
  let byte_off, bit_off = offsets i in
  Format.fprintf fmt "%i (+ %i)" byte_off bit_off

(** Resizes the current buffer so that it big enough to
    represent 'bit_len' bits. *)
let resize t (bit_len : int) =
  Log.debug "[resize] Resize for %i bits@." bit_len;
  let new_size = bytes_needed bit_len in
  Log.debug "[resize] Let's resize with size %i@." new_size;
  let new_buffer = clean_buffer new_size in
  Bytes.blit t.buffer 0 new_buffer 0 (Bytes.length t.buffer);
  t.buffer <- new_buffer

(** Resizes if there is not enough space, does nothing otherwise. *)
let may_resize t (size : int) =
  let buffer_len = Bytes.length t.buffer * 8 in
  let space_left = buffer_len - t.offset - 1 in

  Log.debug "[may_resize] Whole space = %i, Remaining space = %a, size = %a@."
    (buffer_len / 8)
    pp_offset space_left
    pp_offset size;

  if space_left < size then (
    resize t (max (2 * buffer_len) (buffer_len + size))
  )

(** Reads a byte. *)
let read_byte buffer offset =
  Log.debug "[read_byte] Reading byte at position %i@." offset;
  (Bytes.get_int8 buffer offset) land 0xFF

(** Replaces a byte. *)
let set_byte buffer offset byte =
  Log.debug "[set_byte] Writing %a at position %i@." print_int_bin (Int64.of_int byte) offset;
  Bytes.set_int8 buffer offset byte

(** Reads a byte and applies on it a mask *)
let read_byte_mask (buffer : Bytes.t) (i : int) (mask : int -> int) : int =
  let buf_offset, bit_pos = offsets i in
  Log.debug "[read_byte_mask] Analysing byte %i (+ %i)@." buf_offset bit_pos;
  let byt = read_byte buffer buf_offset in
  Log.debug "[read_byte_mask] Read %i (%a)@." byt print_int_bin (Int64.of_int byt);
  let mask = mask bit_pos in
  Log.debug "[read_byte_mask] Applying mask %a@." print_int_bin (Int64.of_int mask);
  byt land mask

(** Returns the bit at position i *)
(* let read_bit (buffer : Bytes.t) (i : int) : int =
 *   let mask bit_pos = (1 lsl 7 - bit_pos) in (\* A 1 at position bit_pos *\)
 *   if read_byte_mask buffer i mask = 0 then 0 else 1 *)

(** Returns the bits after position i *)
let read_byte_right (buffer : Bytes.t) (i : int) : int =
  let mask bit_pos = 255 lsr bit_pos in (* 0s before position bit_pos *)
  read_byte_mask buffer i mask

(** Returns the bits before position i *)
let read_byte_left (buffer : Bytes.t) (i : int) : int =
  (*let _, bit_pos = BI.to_int i in*)
  let mask bit_pos = 255 lsl (7 - bit_pos) in
  (read_byte_mask buffer i mask)

(** Returns 'size' bits starting from 'left'  *)
let read_range (buffer : Bytes.t) (left : int) (size : int) : int64 =
  if size <= 0 || size > 64
  then failwith "Bad size %i" size;
  let right = left + size - 1 in
  Log.debug "[read_range] Read range [%a, %a]@." pp_offset left pp_offset right;
  let left_offset, _ = offsets left in
  let right_offset, right_boffset = offsets right in
  if left_offset = right_offset
  then (
    Log.debug "[read_range] Same byte, leftpos = %a, rightpos = %a@." pp_offset left pp_offset right ;
    let left_part = read_byte_left buffer right in
    let right_part = read_byte_right buffer left in
    Log.debug
      "[read_range] Left byte : %a, Right byte : %a, shift of %i@."
      print_int_bin (Int64.of_int left_part)
      print_int_bin (Int64.of_int right_part)
      (7 - right_boffset);
    Int64.of_int ((left_part land right_part) lsr (7 - right_boffset))
  ) else (
    Log.debug "[read_range] Not on the same byte@.";
    let suffix_size = right_boffset + 1 in
    let left_part = read_byte_right buffer left in
    let right_part = read_byte_left buffer right lsr (8 - suffix_size) in
    Log.debug "[read_range] Left byte : %a, Right byte : %a@."
      print_int_bin (Int64.of_int left_part)
      print_int_bin (Int64.of_int right_part);
    (* Size of suffix is right_boffset + 1 *)
    let () = Log.debug "[read_range] Size of suffix is %i@." suffix_size in
    let suffix = Int64.of_int right_part in
    let rec write (res : int64) (cursor : int) : int64 =
      if cursor = right_offset
      then (
        Log.debug "[read_range] Adding suffix %a@." print_int_bin suffix;
        let res = Int64.(add (shift_left res suffix_size) suffix) in
        Log.debug "[read_range] After adding suffix : %a@." print_int_bin res;
        res
      )
      else (
        let buff_content = read_byte buffer cursor in
        Log.debug "[read_range] Buffer content at curswor %i : %i (%a)@."
          cursor buff_content print_int_bin @@ Int64.of_int buff_content;
        write
          (Int64.(add (shift_left res 8) @@ Int64.of_int buff_content))
          (cursor + 1)
      )
    in

    Log.debug "[read_range] Prefix = %i (%a)@." left_part print_int_bin (Int64.of_int left_part);
    write
      (Int64.of_int left_part)
      (left_offset + 1)
  )

(** Splits 'v' into two integers 'left', 'right' such that the concatenation
    of the binary representation of 'left' and 'right' is 'v'.
    'v' is splitted at position 'pos'.
    Example : split_logical 0b101 4 6 = split_logical 0b000101 4 6 = (0b0001,0b01)
  *)
let split_logical (v : int64) (pos : int) (size : int) =
  Log.debug "[split_logical] Splitting %a of size %i at position %i ->" print_int_bin v size pos;
  assert (pos <= size && pos >= 0);
  let left = Int64.shift_right_logical v (size - pos - 1) in
  let right = Int64.(sub v (shift_left left (size - pos - 1)))
  in
  Log.debug "%a / %a@." print_int_bin left print_int_bin right;
  left, right

(** Writes a byte at the current position of the buffer, and updates
    the buffer offset. The current byte must be empty and the offset
    must start a new byte. *)
let write_chunk (t : t) (v : int) =
  let offset, bioffset = buffer_offsets t in
  let () =
    if bioffset <> 0 then
      failwith "Error : offset %i <> 0" bioffset;
    if v > 255 && v < 0 then
      failwith "Error : value %i not a byte" v;
  (* todo : maybe the invariant of having a clean buffer is not
     necessary.
     In which case, remove this test *)
    if read_byte t.buffer offset <> 0 then
      failwith "There is already %a written at offset %i"
        print_int_bin (read_byte t.buffer offset |> Int64.of_int)
        offset
  in
  set_byte t.buffer offset v;
  t.offset <- t.offset + 8

(** Writes an integer of size i byte per byte. If its size is lower than 8,
    writes it on the first bits and updates the offset. The current offset
    must start a new byte. *)
let rec write_chunks (t : t) (v : int64) (size : int) =
  Log.debug "[write_chunks] Write chunks for %a of size %i@." print_int_bin v size;
  if size <= 8 then begin
    assert (v <= Int64.of_int 255 && v >= Int64.zero);
    let byte = (Int64.to_int v) lsl (8 - size) in
    let current_offset,shouldbezero = buffer_offsets t in
    assert (shouldbezero = 0);
    Log.debug "[write_chunks] Low size : Writing %i at offset %i@." byte current_offset;
    set_byte t.buffer current_offset byte;
    t.offset <- t.offset + size
  end
  else (
    Log.debug "[write_chunks] Size ov %a : %i@." print_int_bin v size;
    let new_byte, new_v = split_logical v 7 size in
    write_chunk t (Int64.to_int new_byte);
    write_chunks t new_v (size - 8)
  )

(** Writes an int64 of size 'size'.
    If the value to write doesn't require overlaying two bytes,
    then it is directly written. Otherwise, the current byte is filled
    and the rest is written with 'write_chunks'. *)
let write_byte (t : t) (v : int64) (size : int) =
  Log.debug
    "[write_byte] Writing %Ld of size %a@." v pp_offset size;
  may_resize t size;
  let neededbytes,boffset = offsets size in
  Log.debug "[write_byte] Needed space : %i (+%i)@." neededbytes boffset;
  (* From now we know that v < 2^size *)
  let t_offset, t_boffset = buffer_offsets t in
  Log.debug "[write_byte] Current offsets : %i (+%i)@." t_offset t_boffset;
  let new_offset = boffset + t_boffset in
  Log.debug "[write_byte] New offset: %i, neededbytes = %i@." new_offset neededbytes;
  if new_offset <= 8 && neededbytes = 0
  then begin
    Log.debug "[write_byte] No need for a new byte@.";
    Log.debug "[write_byte] From boffset %i to %i@." t_boffset new_offset;
    let current_byte = (read_byte t.buffer t_offset) land 255 in
    Log.debug "[write_byte] Current byte = %a@."
      print_int_bin (Int64.of_int current_byte);
    let intv = ((Int64.to_int v) lsl (8-new_offset)) land 255  in
    Log.debug "[write_byte] Adding %a@." print_int_bin (Int64.of_int intv);
    let new_byte = current_byte + intv in
    Log.debug "[write_byte] byte1 : Replacing %a by %a at offset %i@."
      print_int_bin (Int64.of_int current_byte)
      print_int_bin (Int64.of_int new_byte)
      t_offset;
    set_byte t.buffer t_offset new_byte;
    t.offset <- t.offset + size
  end else begin
    Log.debug "[write_byte] Need for at least a new byte@.";
    Log.debug "[write_byte] Whole size = %i@." size;
    let rest_bits = 8 - t_boffset in
    Log.debug "[write_byte] Remaining space in current byte = %i@." rest_bits;
    let left_v, right_v = split_logical v (rest_bits - 1) size in
    Log.debug "[write_byte] Split done@.";
    let current_byte = (read_byte t.buffer t_offset) land 255 in
    Log.debug "[write_byte] Current byte = %a@."
      print_int_bin (Int64.of_int current_byte);
    let new_byte = (Int64.to_int left_v) + current_byte in
    Log.debug "[write_byte] New byte = %a@."
      print_int_bin (Int64.of_int new_byte);
    Log.debug "[write_byte] byte2 : Writing %i at offset %i@." new_byte t_offset;
    set_byte t.buffer t_offset new_byte;
    let previous_offset = t.offset in
    t.offset <- t.offset + 8 - (t.offset mod 8);
    let remaining_size = (size - (t.offset - previous_offset)) in
    Log.debug "[write_byte] New offset = %a. Remaining size : %i@."
      pp_offset t.offset remaining_size;
    write_chunks t right_v remaining_size
  end;
  Log.debug "[write_byte] Ending write. Current offset = %i@." t.offset

let check_size size : unit =
  if size > 64 || size < 0
  then
    invalid_argument "Invalid size %i" size

(** Reads 'size' bits *)
let read (t : reader) (size : int) : int64 =
  Log.debug "[read] Reading %i bits@." size;
  check_size size;
  let buffer = t.reader.buffer in
  let left = t.reader.offset in
  let res = read_range buffer left size in
  t.reader.offset <- t.reader.offset + size;
  Log.debug "[read] New offset = %i@." t.reader.offset;
  res

(** Writes 'size' bits. *)
let write (t : writer) (v : int64) (size : int) =
  Log.debug "[write] Writing %a on %i bits@." print_int_bin v size;
  let () =
    check_size size;

    if v >= (Int64.(shift_left one size))
    then invalid_argument
        "Value %Ld does not fit on %i bits" v size;

    if v < Int64.zero
    then invalid_argument
        "Value %Ld is negative" v
  in
  stats_add_i_bits_int t size;
  write_byte
    t.writer
    v
    size

(** Basic reading and writing utilities. *)
let read_bool   t : bool  = (read t 1) = Int64.one
let read_uint8  t : int   = read t 8  |> Int64.to_int
let read_uint16 t : int   = read t 16 |> Int64.to_int
let read_uint32 t : int   = read t 32 |> Int64.to_int
let read_uint63 t : int64 = read t 63

let write_bool t (b : bool)     = write t Int64.(if b then one else zero) 1
let write_int t (v : int) nbits = write t (Int64.of_int v) nbits
let write_uint8 t (v : int)     = write_int t v 8
let write_uint16 t (v : int)    = write_int t v 16
let write_uint32 t (v : int)    = write_int t v 32
let write_uint63 t (v : int64)  = write t v 63

let z_length value = (Z.numbits value + 1 + 6) / 7

(** Reading a Zarith integer. Zarith integers are unbounded: they are
    represented by chunks 7bits + 1bit stating if the next byte corresponds
    to the same value. *)
let rec read_z value state total_offset =
  Log.debug "[read_z] Continuing the reading@.";
  let there_is_more = read_bool state in
  let byte_content = read state 7 in
  Log.debug "[read_z] First 7 bits = %a\nCurrent value = %a@."
    print_int_bin byte_content
    Z.pp_print value;
  let byte_content = Z.of_int64 byte_content in
  let value =
    let offset_z = Z.(pow (of_int 2) total_offset) in
    let offseted = Z.mul byte_content offset_z in
    Z.(add offseted value) in
  Log.debug "[read_z] New value : %a" Z.pp_print value;
  if there_is_more then
    read_z value state (total_offset + 7)
  else value

let read_z state =
  Log.debug "[read_z] Reading Zarith value at offset %i@." state.reader.offset;
  let there_is_more = read_bool state in
  Log.debug "[read_z] On more than one byte ? %b@." there_is_more ;
  let sign = read_bool state in
  Log.debug "[read_z] Sign: %s@." (if sign then "negative" else "positive");
  let first_value = read state 6 in
  Log.debug "[read_z] First value : %Ld(%a)@." first_value print_int_bin first_value;
  let n =
    if there_is_more then (
      read_z (Z.of_int64 first_value) state 6
    ) else
      Z.of_int64 first_value
  in
  let res = if sign then Z.neg n else n
  in Log.debug "[read_z] Zarith value decoded : %a@." Z.pp_print res; res

(** Writes a Zarith integer.
    Its representation is [|b_0b_1...|
    with:
    - [b_n] where [mod n 8 = 0]: is the current byte the last?
    - [b_1]: encodes the sign.
 *)
let write_z t v =
  Log.debug "[write_z] Writing Zarith %a at position %i@."
    Z.pp_print v
    t.writer.offset;
  let sign = Z.sign v in
  let bits = Z.numbits v in
  let v = Z.abs v in
  (*Log.debug "[write_z] Writing absolute value %a (%a)@." Z.pp_print v
    print_int_bin (Z.to_int64 v); *)
  let get_chunk pos len = Z.to_int64 (Z.extract v pos len) in
  let length = z_length v in
  Log.debug "[write_z] Length = %i. Encoding sign@." length;
  Log.debug "[write_z] Need more than 1 byte ? %b@." (bits > 6);
  write_bool t (bits > 6); (* Does there need more than 1 byte ? *)
  Log.debug "[write_z] Writing sign %s@."
    (if sign < 0 then "negative" else "positive");
  write_bool t (sign < 0); (* Is it negative ? *)
  let six_first = get_chunk 0 6 in
  (* Log.debug "[write_z] Encoding the 6 first bits of %a : %Ld (%a)@."
     print_int_bin (Z.to_int64 v) six_first print_int_bin six_first; *)
  write t six_first 6;
  for i = 1 to length - 1 do
    let pos = 6 + (i - 1) * 7 in
    Log.debug "[write_z] Encoding value : Position %i@." i;
    let there_is_more = (i <> length - 1) in
    Log.debug "[write_z] Will there be more ? %b@." there_is_more;
    write_bool t there_is_more;
    let next_7_bits = get_chunk pos 7 in
    Log.debug "[write_z] Next 7 bits : %a (%Ld)" print_int_bin next_7_bits next_7_bits;
    write t (get_chunk pos 7) 7
  done;
  stats_add_i_bits_z t (length * 8);
  stats_del_i_bits_int t (length * 8)

(* We use a compression format for strings when they are identifiers of
   length < 32. The length is stored on 5 bits, each char on 6 bits. *)
let code__ = int_of_char '_'
let code_0 = int_of_char '0'
let code_a = int_of_char 'a'
let code_A = int_of_char 'A'

let rec is_ident_string s i len =
  i = len ||
  begin
    (match s.[i] with
     | '_'
     | '0'..'9'
     | 'a'..'z'
     | 'A'..'Z' -> true
     | _ -> false
    ) &&
    is_ident_string s (i+1) len
  end

(** Writes an integer of unknown size *)
let write_signed_int t i =
  let zi = Z.of_int i in write_z t zi

(** Writes an integer of unknown size *)
let read_signed_int t =
  let zi = read_z t in
  Z.to_int zi

let write_bytes_known_length ~len t b =
  let exception NotLongEnough in
  try
    for i = 0 to len - 1 do
      let from =
        try Bytes.get_uint8 b i
        with Invalid_argument _ -> raise NotLongEnough
      in
      write_uint8 t from; (* May raise Invalid_argument *)
      stats_add_i_bits_string t 8
    done;
  with
  | NotLongEnough ->
      failwith
        "The buffer to write has size %i, \
         but it is set to fit in %i bytes."
        (Bytes.length b) len

(** Write bytes of any length *)
let write_bytes t b =
  let len = Bytes.length b in
  write_z t (Z.of_int len);
  write_bytes_known_length ~len t b;
  stats_del_i_bits_z t (len * 8)

(** Writes a string of any length *)
let write_string t s =
  let len = String.length s in
  if len < 16 && is_ident_string s 0 len then begin
    write_bool t true;
    write_int t len 4;
    for i = 0 to len - 1 do
      let c = s.[i] in
      let code = int_of_char c in
      let v = match c with
        | '_' -> 0
        | '0'..'9' -> 1 + (code - code_0)
        | 'a'..'z' -> 11 + (code - code_a)
        | 'A'..'Z' -> 37 + (code - code_A)
        | _ -> assert false
      in
      write_int t v 6
    done;
    let stats = 4 + 6*len in
    stats_add_i_bits_string t stats;
    stats_del_i_bits_int t stats
  end else begin
    write_bool t false;
    write_bytes t (Bytes.of_string s)
  end

let read_bytes_known_length ~len t =
  let buff = Bytes.create len in
  for i = 0 to len - 1
  do
    let from = read_uint8 t in
    Bytes.set_int8 buff i from
  done; buff

(** Reads bytes. Its length has been encoded by write_bytes *)
let read_bytes t =
  let len = read_z t |> Z.to_int in
  read_bytes_known_length ~len t

let read_int t nbits = read t nbits |> Int64.to_int

(** Reads a string *)
let read_string t =
  if read_bool t then
    let len = read_int t 4 in
    let buff = Bytes.create len in
    for i = 0 to len - 1 do
      let v = read_int t 6 in
      let c =
        if v < 11 then
          if v = 0 then
            code__
          else
            code_0 + (v - 1)
        else
        if v < 37 then
            code_a + (v - 11)
        else
          code_A + (v - 37)
      in
      Bytes.set_int8 buff i c
    done;
    Bytes.to_string buff
  else Bytes.to_string (read_bytes t)

(** Encodes a string as an integer and keeps track of the
    correspondance between integers and strings. *)
let write_str_repr (t : writer) s =
  match t.dictionary with
  | NoDictionary -> write_string t s
  | Dictionary dict ->
      match StringMap.find_opt s !dict with
      | None ->
          let new_id = StringMap.cardinal !dict in
          dict := StringMap.add s new_id !dict;
          write_signed_int t new_id
      | Some i -> write_signed_int t i

(** Decodes a string encoded as an integer with the reader dictionary. *)
let read_str_repr (t : reader) =
  match t.dictionary with
  | NoDictionary -> read_string t
  | Dictionary d ->
      let id = read_signed_int t in
      match IntMap.find_opt id d with
        None ->
          failwith
            "Binary buffer error: no string associated to %i" id
      | Some s -> s

(** Initializes a writing buffer with an empty buffer, dictionary and stats *)
let initialize_writer ?(with_dict=default_with_dict) ?(init_size = 4096) () =
  { writer =
      {
        buffer = clean_buffer init_size;
        offset = 0;
      };
    dictionary =
      if with_dict
      then Dictionary (ref StringMap.empty)
      else NoDictionary;
    stats = {bits_string = 0; bits_int = 0; bits_z = 0}
  }

(** Encodes and writes the dictionary in a fresh Bytes buffer *)
let write_dict d =
  match d with
  | NoDictionary -> Bytes.empty
  | Dictionary {contents = d} ->
      let t = initialize_writer () in
      let size = StringMap.cardinal d in
      write_signed_int t size;
      let l = StringMap.bindings d |>
              List.fast_sort (fun (_, i1) (_, i2) -> Int.compare i1 i2) in
      List.iter
        (fun (s,i) ->
           Log.debug "[write_dict] %ith word : %s" i s;
           write_string t s)
        l;
      Bytes.sub t.writer.buffer 0 (buffer_size t.writer)

(** Returns the MBytes buffer corresponding to the writer, prefixed by the
    dictionary *)
let finalize t =
  Log.feedback "[finalize]Finalizing: writing the dictionary@.";
  let bdict = write_dict t.dictionary in
  let bres = Bytes.sub t.writer.buffer 0 (buffer_size t.writer) in
  Bytes.concat Bytes.empty [bdict; bres]

(** Initializes a reading buffer by decoding the dictionary at the beginning of the buffer. *)
let initialize_reader ?(with_dict=default_with_dict) buffer =
  Log.feedback "[initialize_reader] Initialization@.";
  let t = {
    reader = {
      buffer;
      offset = 0;
    };
    dictionary = NoDictionary; (* updates later *)
  }
  in
  if with_dict
  then begin
    let dictionary =
      let cardinal = read_signed_int t in
      Log.debug "[initialize_reader] Dictionary size = %i@." cardinal;
      let rec build_dic dict id =
        if id = cardinal
        then (
          let () =
            let byte_offset = t.reader.offset mod 8 in
            if byte_offset = 0 then () else (
              t.reader.offset <- (t.reader.offset + 8 - byte_offset);
            )
          in dict
        )
        else
          let s = read_string t in
          Log.debug "[initialize_reader] Decoded %i <-> %s@." id s;
          build_dic (IntMap.add id s dict) (id + 1)
      in
      build_dic IntMap.empty 0
    in
    t.dictionary <- Dictionary dictionary;
  end;
  Log.debug "[initialize_reader] Reader initialized@.";
  t

let print_stats fmt t =
  let whole = float_of_int @@ (t.stats.bits_string + t.stats.bits_int + t.stats.bits_z)
  in
  let bytes_str = t.stats.bits_string in
  let bytes_int = t.stats.bits_int in
  let bytes_z = t.stats.bits_z in
  Format.fprintf fmt
    "***************Statistics***************\n\
     Strings: %.2f%s (%i/%.0f)\n\
     Integers: %.2f%s (%i/%.0f)\n\
     Zarith: %.2f%s (%i/%.0f)\n"
    ((float_of_int bytes_str) /. whole *. 100.) "%" bytes_str whole
    ((float_of_int bytes_int) /. whole *. 100.) "%" bytes_int whole
    ((float_of_int bytes_z) /. whole *. 100.) "%" bytes_z whole
