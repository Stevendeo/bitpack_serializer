(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2019-2026 OCamlPro                                           *)
(*                                                                            *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* This file is distributed under the terms of the GNU Lesser General Public  *)
(* License version 2.1, with the special exception on linking described in    *)
(* the LICENSE.md file in the root directory.                                 *)
(*                                                                            *)
(******************************************************************************)


open Utils

exception MuReached

type 'a t = | Mu
            | Lens of {
                writer: Buffer.writer -> 'a -> unit;
                reader: Buffer.reader -> 'a;
              }

type _ input =
  | FixedSizeInt64 : int -> int64 input
  | Int64 : int64 input
  | Int : int input
  | ZInt : Z.t input
  | String : string input
  | Bytes : bytes input
  | FixedSizeBytes : int -> bytes input

let write (lens : 'a t) (w_buffer : Buffer.writer) (elt : 'a) : unit =
  match lens with
  | Mu -> raise MuReached
  | Lens {writer; _} -> writer w_buffer elt

let read lens r_buffer =
  match lens with
  | Mu -> raise MuReached
  | Lens {reader; _} -> reader r_buffer

let create (type a) (mode : a input) : a t =
  let (writer : Buffer.writer -> a -> unit) = match mode with
    | FixedSizeInt64 size -> fun w v -> Buffer.write w v size
    | Int64 -> fun w v -> Buffer.write_z w (Z.of_int64 v)
    | Int -> fun w v -> Buffer.write_z w (Z.of_int v)
    | ZInt -> Buffer.write_z
    | String -> Buffer.write_str_repr
    | Bytes -> Buffer.write_bytes
    | FixedSizeBytes len -> Buffer.write_bytes_known_length ~len
  in

  let (reader : Buffer.reader -> a) = match mode with
    | FixedSizeInt64 size -> fun r -> Buffer.read r size
    | Int64 -> fun r -> Buffer.read_z r |> Z.to_int64
    | Int -> fun r -> Buffer.read_z r |> Z.to_int
    | ZInt -> Buffer.read_z
    | String -> Buffer.read_str_repr
    | Bytes -> Buffer.read_bytes
    | FixedSizeBytes len -> Buffer.read_bytes_known_length ~len
  in
  Lens {writer; reader}

let fixed_size_int ~size = create (FixedSizeInt64 size)
let int64 = create Int64
let int = create Int
let zint = create ZInt
let string = create String
let bytes = create Bytes
let fixed_size_bytes ~num_bytes = create (FixedSizeBytes num_bytes)

let conj l1 l2 = Lens {
  writer = (fun w (e1, e2) -> write l1 w e1; write l2 w e2);
  reader = (fun r -> let e1 = read l1 r in let e2 = read l2 r in e1, e2)
}

let list l =
  let rec writer w = function
    | [] -> Buffer.write_bool w false
    | elt :: tl ->
        Buffer.write_bool w true;
        write l w elt;
        writer w tl
  in
  let rec reader acc r =
    if Buffer.read_bool r then
      let new_elt = read l r in
      reader (new_elt :: acc) r
    else
      List.rev acc
  in
  Lens {writer; reader = reader []}

type 'a case =
  | A : {
      destruct : 'a -> 'b option;
      construct : 'b -> 'a;
      lens: 'b t;
    } -> 'a case

let case ~destruct ~construct lens = A {destruct; construct; lens}

let disj (cases : 'a case array) : 'a t =
  let size =
    let len = Array.length cases in
    Utils.numbits len
  in
  let uint_lens = fixed_size_int ~size in
  let writer w e =
    let exception Stop in
    try
      Array.iteri (fun i (A {destruct; lens; _}) ->
          match destruct e with
          | None -> ()
          | Some elt ->
              write uint_lens w (Int64.of_int i); write lens w elt; raise Stop)
        cases;
      failwith "Failing while writing disjunction: case not found."
    with
      Stop -> ()
  in
  let reader r =
    let index = read uint_lens r in
    let A {construct; lens; _} =
      let index = Int64.to_int index in
      try cases.(index) with
      | Invalid_argument _ ->
          failwith "Failing while reading disjunction: \
                    %i is not a valid case identifier (only %i cases)"
            index (Array.length cases)
    in
    read lens r |> construct
  in Lens {writer; reader}

let mu (lens : 'a t -> 'a t) : 'a t =
  let rec writer l w e =
    try write l w e with
    | MuReached -> writer (lens l) w e
  in
  let rec reader l r =
    try read l r with
    | MuReached -> reader (lens l) r
  in
  let init_lens = lens Mu in
  Lens {writer = writer init_lens; reader = reader init_lens}

let trans atob btoa alens =
  let writer w b = write alens w (btoa b) in
  let reader r = atob (read alens r) in
  Lens {writer; reader}
