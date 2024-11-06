(* src/expr.ml *)

(*****************************2.1*********************************)

type pterm =
  | Var of string           (* 变量 *)
  | App of pterm * pterm    (* 函数应用 *)
  | Abs of string * pterm   (* λ抽象 *)

(*****************************2.2*********************************)

(* Pretty printer for pterm *)
let rec print_term (t : pterm) : string =
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"



(*****************************2.3*********************************)

  (* Global counter for generating new variable names *)
let compteur_var : int ref = ref 0

(* Function to generate a new variable name *)
let nouvelle_var () : string =
  let _ = compteur_var := !compteur_var + 1 in
  "x" ^ (string_of_int !compteur_var)

(* Alpha-conversion function to rename bound variables *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x
  | App (t1, t2) -> App (alphaconv t1, alphaconv t2)
  | Abs (x, t) ->
      let new_var = nouvelle_var () in
      let rec substitute v new_v term =
        match term with
        | Var y -> if y = v then Var new_v else Var y
        | App (t1, t2) -> App (substitute v new_v t1, substitute v new_v t2)
        | Abs (y, t) ->
            if y = v then Abs (y, t)
            else Abs (y, substitute v new_v t)
      in
      Abs (new_var, substitute x new_var (alphaconv t))


(*

代码说明
compteur_var：一个引用类型的全局计数器，用于生成新的变量名。
nouvelle_var：生成新的变量名，每次调用会递增 compteur_var。
alphaconv：递归地对表达式 t 进行 alpha-conversion。
对于变量和应用，直接递归处理。
对于抽象，将绑定变量 x 重命名为新变量 new_var，并替换函数体中的所有出现。
此代码会递归地重命名所有绑定的变量以避免命名冲突。完成这部分后，我们可以继续添加其他功能。

*)


(*****************************2.4*********************************)   

(* Substitution function that replaces all free occurrences of a variable with a term *)
let rec substitution (x : string) (n : pterm) (t : pterm) : pterm =
  match t with
  | Var y -> if y = x then n else Var y
  | App (t1, t2) -> App (substitution x n t1, substitution x n t2)
  | Abs (y, t1) ->
      if y = x then
        Abs (y, t1)  (* Bound variable, no substitution inside this scope *)
      else if List.mem y (free_vars n) then
        let z = nouvelle_var () in
        Abs (z, substitution x n (substitution y (Var z) t1))
      else
        Abs (y, substitution x n t1)

(* Helper function to get free variables in a term *)
and free_vars (t : pterm) : string list =
  match t with
  | Var x -> [x]
  | App (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Abs (x, t1) -> List.filter (fun y -> y <> x) (free_vars t1)


(*
代码说明
substitution：

如果是变量 Var y，并且 y 与 x 相同，则替换为 n，否则返回 Var y。
对于应用 App (t1, t2)，递归地对 t1 和 t2 进行替换。
对于抽象 Abs (y, t1)，如果 y 是绑定变量并且等于 x，则不进行替换。
如果 y 在 n 中是自由的，则使用 nouvelle_var 生成一个新变量 z 并替换 y 为 z，以避免变量捕获。
free_vars：辅助函数，用于获取表达式中的自由变量列表。

这样就实现了替换功能，可以避免变量捕获问题。完成这部分后，我们可以继续下一步。
*)



(*****************************2.5*********************************) 


(* One-step LtR-CbV reduction function *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* 变量无法简化 *)
  | Abs (x, t1) -> (
      match ltr_cbv_step t1 with
      | Some t1' -> Some (Abs (x, t1'))
      | None -> None
    )
  | App (Abs (x, t1), v2) when is_value v2 -> 
      Some (substitution x v2 t1)
  | App (v1, t2) when is_value v1 -> (
      match ltr_cbv_step t2 with
      | Some t2' -> Some (App (v1, t2'))
      | None -> None
    )
  | App (t1, t2) -> (
      match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> None
    )

(* Helper function to check if a term is a value (i.e., an abstraction) *)
and is_value (t : pterm) : bool =
  match t with
  | Abs (_, _) -> true
  | _ -> false


(*
代码说明
ltr_cbv_step：这个递归函数实现了 LtR-CbV 简化策略。

对于变量（Var），不能再简化，返回 None。
对于抽象（Abs），尝试对其主体 t1 进行简化。
对于应用（App (Abs (x, t1), v2)），如果 v2 是值（即另一个抽象），则直接进行 β 简化，即替换 x 为 v2。
如果左侧是值 v1，则尝试简化右侧 t2。
否则，尝试简化左侧 t1。
is_value：辅助函数，用于检查一个表达式是否是值（即抽象）。

此函数实现了一步的 LtR-CbV 简化，返回的结果是一个 pterm option，如果不能再简化，则返回 None。完成这部分后，我们可以继续实现其他功能。  

*)


(*****************************2.6*********************************)


(* Normalization function using LtR-CbV strategy *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_cbv_step t with
  | Some t' -> ltr_cbv_norm t'
  | None -> t

(* Normalization function with timeout mechanism *)
let ltr_cbv_norm_with_timeout (t : pterm) (timeout : float) : pterm option =
  let start_time = Unix.gettimeofday () in
  let rec aux term =
    if Unix.gettimeofday () -. start_time > timeout then
      None  (* 超时返回 None *)
    else
      match ltr_cbv_step term with
      | Some term' -> aux term'
      | None -> Some term
  in
  aux t


(*

代码说明
ltr_cbv_norm：该递归函数会一直应用 ltr_cbv_step 进行简化，直到无法再简化为止。当 ltr_cbv_step 返回 None 时，即表示达到了正常形式，返回最终的表达式。
ltr_cbv_norm_with_timeout：该函数增加了一个超时机制，以防止无限循环。如果在给定的 timeout 时间内无法达到正常形式，则返回 None，表示计算超时。我们使用 Unix.gettimeofday 来记录开始时间，并在每次递归调用时检查当前时间与开始时间的差值是否超过了 timeout。
这样就实现了一个带有 LtR-CbV 策略的规范化函数，以及一个带有超时控制的版本。完成这部分后，如果有其他需求，可以继续添加。
*)  



(*****************************2.7*********************************)

(* src/expr.ml *)

(* Common lambda calculus terms *)

(* Identity function I *)
let i_term = Abs ("x", Var "x")

(* Delta function δ (delta) *)
let delta_term = Abs ("x", App (Var "x", Var "x"))

(* Omega function Ω *)
let omega_term = App (delta_term, delta_term)

(* Combinator K *)
let k_term = Abs ("x", Abs ("y", Var "x"))

(* Combinator S *)
let s_term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))

(* SKK expression *)
let skk_term = App (App (s_term, k_term), k_term)

(* SKI I combinator *)
let ski_i_term = App (App (s_term, k_term), i_term)

(* Church numerals for 0, 1, 2, and 3 *)
let zero = Abs ("f", Abs ("x", Var "x"))
let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))
let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))
let three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))

(* Addition (λm.λn.λf.λx.m f (n f x)) *)
let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))

(* Multiplication (λm.λn.λf.m (n f)) *)
let mul = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))

(* Successor (λn.λf.λx.f (n f x)) *)
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))

(* Testing normalization function on the examples *)
let () =
  let examples = [
    ("I", i_term);
    ("δ", delta_term);
    ("Ω", omega_term);
    ("S", s_term);
    ("K", k_term);
    ("SKK", skk_term);
    ("S K I I", ski_i_term);
    ("0", zero);
    ("1", one);
    ("2", two);
    ("3", three);
    ("Addition (1 + 2)", App (App (add, one), two));
    ("Multiplication (2 * 3)", App (App (mul, two), three));
    ("Successor of 1", App (succ, one));
  ] in
  List.iter (fun (name, term) ->
    let result = ltr_cbv_norm term in
    Printf.printf "%s: %s\n" name (print_term result)
  ) examples


(*
  代码说明
定义常见的 λ 演算项：

i_term：恒等函数 
𝐼
I。
delta_term 和 omega_term：德尔塔和 Omega 函数。
k_term 和 s_term：组合子 
𝐾
K 和 
𝑆
S。
skk_term 和 ski_i_term：组合子应用。
zero，one，two，three：教会数表示 0，1，2，和 3。
add，mul，succ：基本算术运算（加法、乘法、后继）。
测试规范化函数：

我们将每个定义的项与 ltr_cbv_norm 一起运行，并输出其最终的标准形式。
运行该代码时，将会输出每个示例项的规范化结果。完成这部分后，您可以根据输出检查规范化函数的正确性。
  *)