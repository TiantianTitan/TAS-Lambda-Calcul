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
  Printf.printf "Generating equations for term...\n";  (* 调试输出 *)
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

(*****************************3.4*********************************)

(* Occur check function to verify if a variable occurs within a type *)
  let rec occur_check (var : string) (t : ptype) : bool =
    match t with
    | Var x -> x = var
    | Arr (t1, t2) -> occur_check var t1 || occur_check var t2
    | Nat -> false


(*
代码说明
occur_check var t：该函数检查类型 t 中是否包含类型变量 var。
对于 Var x，如果 x 等于 var，返回 true。
对于 Arr (t1, t2)，递归检查 t1 和 t2，如果任一部分包含 var，则返回 true。
对于 Nat 类型，返回 false，因为 Nat 类型不包含任何变量。
使用场景
这个函数在类型推导的过程中会用到，特别是在进行类型替换时，用于防止循环依赖（例如，当一个类型变量直接或间接出现在它自身的定义中）。
*)

(*****************************3.5*********************************)


(* Substitute a type variable with a given type within another type *)
let rec substitute_type (var : string) (replacement : ptype) (t : ptype) : ptype =
  match t with
  | Var x -> if x = var then replacement else Var x
  | Arr (t1, t2) -> Arr (substitute_type var replacement t1, substitute_type var replacement t2)
  | Nat -> Nat

(* Substitute a type variable with a given type within a system of equations *)
let substitute_in_equations (var : string) (replacement : ptype) (equations : equa) : equa =
  List.map (fun (t1, t2) -> 
    (substitute_type var replacement t1, substitute_type var replacement t2)
  ) equations


(*
代码说明
substitute_type：用于在单个类型 t 中将类型变量 var 替换为指定的类型 replacement。

如果 t 是 Var x 并且 x 等于 var，则返回 replacement。
如果 t 是箭头类型 Arr (t1, t2)，则递归地在 t1 和 t2 中进行替换。
如果 t 是 Nat，则直接返回 Nat，因为 Nat 不包含类型变量。
substitute_in_equations：在类型方程系统 equations 中进行替换。

对于每个方程 (t1, t2)，在 t1 和 t2 中应用 substitute_type 函数替换 var，并返回新的方程列表。
这两个函数将帮助我们在类型推导过程中处理类型变量替换。完成这部分后，我们可以继续实现其他功能。
*)


(*****************************3.6*********************************)

(* Perform one step of unification on a system of equations *)
let rec unify_step (equations : equa) : equa option =
  match equations with
  | [] -> Some []  (* 如果没有方程，返回成功的空系统 *)
  | (t1, t2) :: rest ->
      if t1 = t2 then
        (* 情况 1: 两个类型相等，删除该方程 *)
        unify_step rest
      else
        match (t1, t2) with
        | (Var x, _) ->
            if not (occur_check x t2) then
              (* 情况 2: 左侧是变量且不出现在右侧 *)
              let substituted_rest = substitute_in_equations x t2 rest in
              unify_step substituted_rest
            else
              None  (* 发生循环引用，失败 *)
        | (_, Var y) ->
            if not (occur_check y t1) then
              (* 情况 2: 右侧是变量且不出现在左侧 *)
              let substituted_rest = substitute_in_equations y t1 rest in
              unify_step substituted_rest
            else
              None  (* 发生循环引用，失败 *)
        | (Arr (t1a, t1b), Arr (t2a, t2b)) ->
            (* 情况 3: 两边都是箭头类型 *)
            unify_step ((t1a, t2a) :: (t1b, t2b) :: rest)
        | _ -> None  (* 情况 4: 无法统一，返回失败 *)


(*
代码说明
情况 1：如果方程的两边 t1 和 t2 相等，则删除该方程，继续处理剩余的方程。
情况 2：如果 t1 或 t2 是变量，并且该变量不在另一边中出现：
使用 substitute_in_equations 将该变量替换为另一边的类型。
然后在更新后的方程系统上递归调用 unify_step。
情况 3：如果 t1 和 t2 都是箭头类型，则将其分解为两个新的方程，并加入到方程系统中。
情况 4：如果以上条件都不满足，则返回 None，表示无法统一。
这个 unify_step 函数执行一次统一步骤，并在方程系统中逐步解决类型变量和结构。如果整个系统都能简化为一个空系统，则表示成功，否则如果遇到失败情况则返回 None。

完成这部分后，我们可以继续实现一个递归调用的 unify 函数，用于多步执行直到无法进一步简化。
*)

(*****************************3.7*********************************)

open Unix

(* Solve a system of equations with a timeout *)
let solve_with_timeout (equations : equa) (timeout : float) : equa option =
  let start_time = Unix.gettimeofday () in
  let rec solve eqs depth =
    Printf.printf "Depth: %d, Equations count: %d\n" depth (List.length eqs); (* 添加调试信息 *)
    if Unix.gettimeofday () -. start_time > timeout then
      None  (* 超时，返回 None *)
    else
      match unify_step eqs with
      | None -> None  (* 无法统一，返回 None *)
      | Some [] -> Some []  (* 所有方程已解决，返回空系统，表示成功 *)
      | Some new_eqs -> solve new_eqs (depth + 1)  (* 递归地解决新方程系统 *)
  in
  solve equations 0



(* Infer the type of a term *)
let infer_type (term : pterm) (timeout : float) : ptype option =
  Printf.printf "Starting type inference...\n"; (* 调试输出 *)
  let ty_var = Var (nouvelle_var_t ()) in
  let equations = genere_equa term ty_var [] in
  match solve_with_timeout equations timeout with
  | None -> Printf.printf "Type inference failed.\n"; None  (* 失败时的调试输出 *)
  | Some solved_eqs ->
      Printf.printf "Type inference succeeded.\n";  (* 成功时的调试输出 *)
      (* 使用已解决的方程系统替换类型变量 *)
      let rec apply_solutions ty eqs =
        match eqs with
        | [] -> ty
        | (Var x, t) :: rest -> apply_solutions (substitute_type x t ty) rest
        | _ :: rest -> apply_solutions ty rest
      in
      Some (apply_solutions ty_var solved_eqs)

(*
代码说明
solve_with_timeout：在超时机制下解决方程系统。

记录开始时间，并在每次递归调用时检查超时。
如果 unify_step 返回 None，则系统不可解，返回 None。
如果方程系统被解决为 []，则返回空系统，表示成功。
infer_type：

生成类型方程系统并尝试在超时内解决。
如果解决成功，则应用解决后的方程系统来替换类型变量。
如果超时或系统不可解，则返回 None，表示该项不可类型化。
通过这些步骤，infer_type 函数可以在指定的时间内尝试推导项的类型，并返回成功的类型或不可类型化的结果。完成这些后，您可以使用示例项测试该类型推导函数。
*)

(*****************************3.8*********************************)