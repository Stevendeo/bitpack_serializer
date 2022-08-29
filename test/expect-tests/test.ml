open Binary_encoding.Main

let write_read with_dict w r =
  let writer = initialize_writer ~with_dict () in
  let () = w writer in
  let buff = finalize writer in
  let reader = initialize_reader ~with_dict buff in
  r reader, Bytes.length buff

let%expect_test "int_encoding" =
  let w writer = write writer 3L 2 in
  let r reader = read reader 2 in
  let res, size = write_read false w r in
  Format.printf "%Ld, %i" res size;
  [%expect {| 3, 1 |}]
