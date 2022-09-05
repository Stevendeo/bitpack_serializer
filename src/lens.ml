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

exception MuReached

type 'a t = | Mu
            | Lens of {
                writer: Buffer.writer -> 'a -> unit;
                reader: Buffer.reader -> 'a;
              }

type _ input =
  | UInt : int -> int64 input (* Size *)
  | SInt : int input
  | ZInt : Z.t input
  | String : string input

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
    | UInt size -> fun w v -> Buffer.write w v size
    | SInt -> fun w v -> Buffer.write_z w (Z.of_int v)
    | ZInt -> fun w v -> Buffer.write_z w v
    | String -> fun w v -> Buffer.write_str_repr w v
  in

  let (reader : Buffer.reader -> a) = match mode with
    | UInt size -> fun r -> Buffer.read r size
    | SInt -> fun r -> Buffer.read_z r |> Z.to_int
    | ZInt -> fun r -> Buffer.read_z r
    | String -> fun r -> Buffer.read_str_repr r
  in
  Lens {writer; reader}

let uint ~size = create (UInt size)
let sint = create SInt
let zint = create ZInt
let string = create String
let conj l1 l2 = Lens {
  writer = (fun w (e1, e2) -> write l1 w e1; write l2 w e2);
  reader = (fun r -> let e1 = read l1 r in let e2 = read l2 r in e1, e2)
}

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
  let uint_lens = uint ~size in
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
    let A {construct; lens; _} = cases.(Int64.to_int index) in
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
