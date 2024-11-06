(* src/types.ml *)

open Expr

(*****************************3.1*********************************)

(* Simple types for lambda calculus *)
type ptype =
  | Var of string         (* 类型变量 *)
  | Arr of ptype * ptype  (* 箭头类型，表示 T -> T *)
  | Nat                   (* 自然数类型 *)

(*
代码说明
Var of string：表示类型变量，类似于泛型类型变量（例如 T, U 等）。
Arr of ptype * ptype：表示箭头类型，即函数类型。Arr (t1, t2) 表示从类型 t1 到类型 t2 的函数类型。
Nat：表示自然数类型。
这样，我们就定义了 λ 演算中简单类型的 AST 表示，可以在类型检查器或类型推断系统中使用这些类型结构。
*)


(*****************************3.2*********************************)


(* Pretty printer for ptype *)
let rec print_type (t : ptype) : string =
  match t with
  | Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | Nat -> "Nat"


  (*
  代码说明
对于 Var x，直接返回变量名 x。
对于箭头类型 Arr (t1, t2)，递归调用 print_type，格式化为 (t1 -> t2)。
对于自然数类型 Nat，返回字符串 "Nat"。

这样，我们就完成了一个简单的类型 pretty printer，可以用于输出和调试
  *)


(*****************************3.3*********************************)


  (* Global counter for generating fresh type variables *)
let compteur_var_t : int ref = ref 0

(* Function to generate a new type variable *)
let nouvelle_var_t () : string =
  let _ = compteur_var_t := !compteur_var_t + 1 in
  "T" ^ (string_of_int !compteur_var_t)

(* Type equation type *)
type equa = (ptype * ptype) list

(* Environment type *)
type env = (string * ptype) list

(* Function to search for a type in the environment *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith ("Type for variable " ^ v ^ " not found in environment.")
  | (var, t) :: rest -> if var = v then t else cherche_type v rest

(* Function to generate type equations from a term *)
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
  | Var v ->
      let tv = cherche_type v e in
      [(tv, ty)]
  | App (t1, t2) ->
      let ta = Var (nouvelle_var_t ()) in
      let eq1 = genere_equa t1 (Arr (ta, ty)) e in
      let eq2 = genere_equa t2 ta e in
      eq1 @ eq2
  | Abs (x, t) ->
      let ta = Var (nouvelle_var_t ()) in
      let tb = Var (nouvelle_var_t ()) in
      let eq_body = genere_equa t tb ((x, ta) :: e) in
      (ty, Arr (ta, tb)) :: eq_body


(*
代码说明
计数器和新类型变量生成：

compteur_var_t 是一个全局计数器，用于生成新的类型变量。
nouvelle_var_t 函数每次调用时生成一个唯一的类型变量，例如 T1、T2 等。
类型方程和环境类型：

type equa 定义为 (ptype * ptype) list，表示类型方程列表。
type env 定义为 (string * ptype) list，表示变量的环境映射，每个变量关联一个类型。
cherche_type：

从环境中查找变量的类型。如果找不到则抛出错误。
genere_equa：

递归地生成类型方程。
变量（Var v）：查找变量 v 的类型 tv，生成方程 tv = ty。
应用（App (t1, t2)）：为 t2 创建一个新的类型变量 ta，生成两个方程：t1 应该是从 ta 到 ty 的函数类型，以及 t2 的类型应该是 ta。
抽象（Abs (x, t)）：为 x 的类型生成新的类型变量 ta，为返回类型生成新的类型变量 tb，并在扩展环境中递归调用 genere_equa 以生成 t 的类型方程。
这样就定义了生成类型方程的机制。完成这部分后，我们可以继续进行类型推断或类型检查的下一步。
*)