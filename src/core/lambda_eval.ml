
open Lambda_ast

(*opérations de mémoire de base*)
let rec lookup_memory (address: int) (memory: memory): lambda_expr option =
  try Some (List.assoc address memory) with Not_found -> None

and update_memory (address: int) (value: lambda_expr) (memory: memory): memory =
  (address, value) :: List.remove_assoc address memory

and add_to_memory (value: lambda_expr) (memory: memory): (int * memory) =
  let addr = new_memory_address () in
  let updated_memory = update_memory addr value memory in
  (addr, updated_memory)

and memory_to_string (memory: memory): string =
  let bindings_to_string (addr, expr) =
    "(" ^ string_of_int addr ^ ", " ^ lambda_expr_to_string expr ^ ")"
  in
  "[" ^ String.concat ", " (List.map bindings_to_string memory) ^ "]"

(* Déterminez s'il s'agit d'une valeur *)
let rec is_value (expr: lambda_expr): bool =
  match expr with
  | Integer _ -> true
  | UnitValue -> true
  | Abstraction _ -> true
  | LambdaList _ -> true
  | MemoryAddress _ -> true
  | _ -> false

(*Stratégie d'évaluation Call-by-Value de gauche à droite*)
let rec eval_step (expr: lambda_expr) (memory: memory): (lambda_expr * memory) option =
  match expr with
(*variable*)
  | Variable _ -> None

(* Application (β-Réduction) *)
  | Application (Abstraction (param, body), arg) when is_value arg ->
      Some (substitute param arg body, memory)

  | Application (func, arg) -> (
      match eval_step func memory with
      | Some (func', memory') -> Some (Application (func', arg), memory')
      | None -> (
          match eval_step arg memory with
          | Some (arg', memory') -> Some (Application (func, arg'), memory')
          | None -> None
        )
    )

(*opérations arithmétiques*)
  | Addition (Integer a, Integer b) -> Some (Integer (a + b), memory)
  | Addition (e1, e2) -> (
      match eval_step e1 memory with
      | Some (e1', memory') -> Some (Addition (e1', e2), memory')
      | None -> (
          match eval_step e2 memory with
          | Some (e2', memory') -> Some (Addition (e1, e2'), memory')
          | None -> None
        )
    )

  | Subtraction (Integer a, Integer b) -> Some (Integer (a - b), memory)
  | Subtraction (e1, e2) -> (
      match eval_step e1 memory with
      | Some (e1', memory') -> Some (Subtraction (e1', e2), memory')
      | None -> (
          match eval_step e2 memory with
          | Some (e2', memory') -> Some (Subtraction (e1, e2'), memory')
          | None -> None
        )
    )

  | Multiplication (Integer a, Integer b) -> Some (Integer (a * b), memory)
  | Multiplication (e1, e2) -> (
      match eval_step e1 memory with
      | Some (e1', memory') -> Some (Multiplication (e1', e2), memory')
      | None -> (
          match eval_step e2 memory with
          | Some (e2', memory') -> Some (Multiplication (e1, e2'), memory')
          | None -> None
        )
    )

(* lister les opérations *)
  | Head (LambdaList (Node (head, _))) -> Some (head, memory)
  | Head expr -> (
      match eval_step expr memory with
      | Some (expr', memory') -> Some (Head expr', memory')
      | None -> None
    )

  | Tail (LambdaList (Node (_, tail))) -> Some (LambdaList tail, memory)
  | Tail expr -> (
      match eval_step expr memory with
      | Some (expr', memory') -> Some (Tail expr', memory')
      | None -> None
    )

(*branche conditionnelle*)
  | IfZero (Integer 0, cons, _) -> Some (cons, memory)
  | IfZero (Integer _, _, alt) -> Some (alt, memory)
  | IfZero (cond, cons, alt) -> (
      match eval_step cond memory with
      | Some (cond', memory') -> Some (IfZero (cond', cons, alt), memory')
      | None -> None
    )

  | IfNil (LambdaList Nil, cons, _) -> Some (cons, memory)
  | IfNil (LambdaList _, _, alt) -> Some (alt, memory)
  | IfNil (cond, cons, alt) -> (
      match eval_step cond memory with
      | Some (cond', memory') -> Some (IfNil (cond', cons, alt), memory')
      | None -> None
    )

  (* Fix-point *)
  | FixPoint (Abstraction (param, body)) ->
      Some (substitute param expr body, memory)
  | FixPoint expr -> (
      match eval_step expr memory with
      | Some (expr', memory') -> Some (FixPoint expr', memory')
      | None -> None
    )

(* Type de référence (Ref, Deref, Assign) *)
  | RefValue value when is_value value ->
      let addr, updated_memory = add_to_memory value memory in
      Some (MemoryAddress addr, updated_memory)
  | RefValue value -> (
      match eval_step value memory with
      | Some (value', memory') -> Some (RefValue value', memory')
      | None -> None
    )

  | Deref (MemoryAddress addr) -> (
      match lookup_memory addr memory with
      | Some value -> Some (value, memory)
      | None -> failwith ("Invalid memory access at address " ^ string_of_int addr)
    )
  | Deref expr -> (
      match eval_step expr memory with
      | Some (expr', memory') -> Some (Deref expr', memory')
      | None -> None
    )

  | Assign (MemoryAddress addr, value) when is_value value ->
      let updated_memory = update_memory addr value memory in
      Some (UnitValue, updated_memory)
  | Assign (expr1, expr2) -> (
      match eval_step expr1 memory with
      | Some (expr1', memory') -> Some (Assign (expr1', expr2), memory')
      | None -> (
          match eval_step expr2 memory with
          | Some (expr2', memory') -> Some (Assign (expr1, expr2'), memory')
          | None -> None
        )
    )

(*autres cas*)
  | _ -> None

(* Évaluez de manière récursive jusqu'à ce que vous obteniez le formulaire standard *)
let rec eval_to_normal_form (expr: lambda_expr) (memory: memory): (lambda_expr * memory) =
  match eval_step expr memory with
  | Some (next_expr, updated_memory) -> eval_to_normal_form next_expr updated_memory
  | None -> (expr, memory)

(*variable de remplacement*)
and substitute (var: string) (value: lambda_expr) (expr: lambda_expr): lambda_expr =
  match expr with
  | Variable x when x = var -> value
  | Variable _ -> expr
  | Application (e1, e2) -> Application (substitute var value e1, substitute var value e2)
  | Abstraction (param, body) when param <> var -> Abstraction (param, substitute var value body)
  | Addition (e1, e2) -> Addition (substitute var value e1, substitute var value e2)
  | Subtraction (e1, e2) -> Subtraction (substitute var value e1, substitute var value e2)
  | Multiplication (e1, e2) -> Multiplication (substitute var value e1, substitute var value e2)
  | LambdaList lst -> LambdaList (substitute_in_list var value lst)
  | Head e -> Head (substitute var value e)
  | Tail e -> Tail (substitute var value e)
  | IfZero (cond, cons, alt) -> IfZero (substitute var value cond, substitute var value cons, substitute var value alt)
  | IfNil (cond, cons, alt) -> IfNil (substitute var value cond, substitute var value cons, substitute var value alt)
  | FixPoint e -> FixPoint (substitute var value e)
  | LetBinding (x, e1, e2) when x <> var ->
      LetBinding (x, substitute var value e1, substitute var value e2)
  | RefValue e -> RefValue (substitute var value e)
  | Deref e -> Deref (substitute var value e)
  | Assign (e1, e2) -> Assign (substitute var value e1, substitute var value e2)
  | _ -> expr

and substitute_in_list (var: string) (value: lambda_expr) (lst: lambda_expr_list): lambda_expr_list =
  match lst with
  | Nil -> Nil
  | Node (head, tail) -> Node (substitute var value head, substitute_in_list var value tail)
