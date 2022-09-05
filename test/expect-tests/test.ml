open Binary_encoding
open Buffer

let write_read with_dict w r elt =
  let writer = initialize_writer ~with_dict () in
  let () = w writer elt in
  let buff = finalize writer in
  (* print_stats Format.std_formatter writer; *)
  let reader = initialize_reader ~with_dict buff in
  r reader, Bytes.length buff

let%expect_test "int_encoding" =
  let to_encode = 3L in
  let w writer elt = write writer elt 2 in
  let r reader = read reader 2 in
  let res, _size = write_read NoDictionary w r to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]


type operation =
  | Var of string
  | Const of int
  | Add of operation * operation
  | Mul of operation * operation
  | Sub of operation * operation

let%expect_test "lens" =
  let open Lens in
  let operation_lens = mu @@ fun self ->
    disj  [|
      case
        ~destruct:(function Var v -> Some v | _ -> None)
        ~construct:(fun v -> Var v)
        string;
      case
        ~destruct:(function Const i -> Some i | _ -> None)
        ~construct:(fun i -> Const i)
        sint;
      case
        ~destruct:(function Add (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Add (o1, o2))
        (conj self self);
      case
        ~destruct:(function Mul (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Mul (o1, o2))
        (conj self self);
      case
        ~destruct:(function Sub (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Sub (o1, o2))
        (conj self self);
    |] in
  let v =
    Add(
      Add (Var "toto", Const 4),
      Mul (
        Sub (Var "toto", Var "toto"),
        Const 0
      )
    ) in
  let res, _size =
    write_read (Dictionary (Some 3))
      (Lens.write operation_lens)
      (Lens.read operation_lens)
      v
  in
  Format.printf "%b" (res = v);
  [%expect {| true |}]

