open Test_typing

let () =
  Printf.printf "Starting test_typing program...\n";
  Printf.printf "Testing type inference:\n";
  test_typing i_term "I (Identity)";
