(* src/test_typing.ml *)

(*****************************3.8*********************************)

open Types  (* 确保 types.ml 定义了 infer_type 等函数 *)
open Expr   (* 确保 expr.ml 定义了 pterm 类型和一些基本项 *)

(* 测试项和超时参数 *)
let timeout = 2.0

(* 定义一些测试项 *)

(* 恒等函数 I: 应返回类型 T -> T *)
let i_term = Abs ("x", Var "x")

(* 组合子 K: 应返回类型 T1 -> (T2 -> T1) *)
let k_term = Abs ("x", Abs ("y", Var "x"))

(* 组合子 S: 应返回类型 (T -> U -> V) -> (T -> U) -> (T -> V) *)
let s_term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))

(* Omega 项 (Ω): 非终止项，无法推导类型 *)
let omega_term = App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x")))

(* 自然数 0, 1, 2 的教会数表示: 应返回类型 (T -> T) -> T -> T *)
let zero = Abs ("f", Abs ("x", Var "x"))
let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))
let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))

(* 算术运算 *)
let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
let mul = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))

(* 定义测试函数 *)
let test_typing term name =
  match infer_type term timeout with
  | None -> Printf.printf "%s: Not typable\n" name
  | Some ty -> Printf.printf "%s: %s\n" name (print_type ty)

(* 执行测试 *)
let () =
  Printf.printf "Testing type inference:\n";
  (* test_typing i_term "I (Identity)";
  test_typing k_term "K (Combinator)";
  test_typing s_term "S (Combinator)";
  test_typing omega_term "Ω (Omega, should timeout or fail)";
  test_typing zero "0 (Church numeral)";
  test_typing one "1 (Church numeral)";
  test_typing two "2 (Church numeral)";
  test_typing (App (App (add, one), two)) "Addition (1 + 2)";
  test_typing (App (App (mul, two), two)) "Multiplication (2 * 2)";
  test_typing (App (succ,one)) "Successor of 1"; *)
