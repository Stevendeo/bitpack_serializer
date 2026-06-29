(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2023 OcamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module Buffer = Bitpack_serializer.Buffer
module Lens = Bitpack_serializer.Lens

let%expect_test "maxint_encoding" =
  let to_encode = max_int in
  let config = Lib.{lens = Lens.int; dictionary = `NoDictionary} in
  let res, _size = Lib.write_read config to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]

let%expect_test "minint_encoding" =
  let to_encode = min_int in
  let config = Lib.{lens = Lens.int; dictionary = `NoDictionary} in
  let res, _size = Lib.write_read config to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]

let%expect_test "uint_encoding" =
  let to_encode = Int64.max_int in
  let config = Lib.{lens = Lens.fixed_size_int ~size:64; dictionary = `NoDictionary} in
  let res, _size = Lib.write_read config to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]

let%expect_test "min_int64_encoding" =
  let to_encode = Int64.min_int in
  let config = Lib.{lens = Lens.fixed_size_int ~size:64; dictionary = `NoDictionary} in
  let res, _size = Lib.write_read config to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]

let%expect_test "max_int64_encoding" =
  let to_encode = Int64.max_int in
  let config = Lib.{lens = Lens.fixed_size_int ~size:64; dictionary = `NoDictionary} in
  let res, _size = Lib.write_read config to_encode in
  Format.printf "%b" (res = to_encode);
  [%expect {| true |}]

let modules_to_test = [
  (module Test_operation : Sig.S);
  (module Test_records : Sig.S);
  (module Test_lists : Sig.S);
]

let%expect_test _ =
  List.iter
    (fun (module To_test : Sig.S) ->
       List.iter
         (fun (elt, config) ->
            let res, _ = Lib.write_read config elt in
            if not (To_test.eq elt res) then begin
              Format.printf "Error while testing %s:\n%a\n is not equal to\n%a@."
                To_test.id
                To_test.pp elt
                To_test.pp res
            end
         )
         To_test.to_test
    )
    modules_to_test
