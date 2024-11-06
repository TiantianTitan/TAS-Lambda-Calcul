

(* src/test_typing.ml *)

open Expr
open Types

let timeout = 10.0


(* 测试项 *)
let i_term = Abs ("x", Var "x")            (* 恒等函数 I *)
let k_term = Abs ("x", Abs ("y", Var "x"))  (* 组合子 K *)
let s_term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))) (* 组合子 S *)

(* 自然数表示 *)
let zero = Abs ("f", Abs ("x", Var "x"))
let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))
let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))

(* 算术操作 *)
let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))

(* 测试函数 *)
let test_typing term name =
  match infer_type term timeout with
  | None -> Printf.printf "%s: Not typable\n" name
  | Some ty -> Printf.printf "%s: %s\n" name (print_type ty)

(* 执行测试 *)

let () =
  Printf.printf "Testing type inference:\n";
  (* test_typing i_term "I (Identity)"; *)

(* let () =
  Printf.printf "Testing type inference:\n";
  test_typing i_term "I (Identity)";
  test_typing k_term "K (Combinator)";
  test_typing s_term "S (Combinator)";
  test_typing zero "0 (Church numeral)";
  test_typing one "1 (Church numeral)";
  test_typing two "2 (Church numeral)";
  test_typing (App (App (add, one), two)) "Addition (1 + 2)";
  test_typing (App (succ,one)) "Successor of 1"; *)
