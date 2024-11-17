open Lambda_ast


(* Fonction pour calculer la longueur d'une liste *)
let rec list_length (lst: lambda_expr_list): int =
  match lst with
  | Nil -> 0
  | Node (_, tail) -> 1 + list_length tail


(* Opérations de mémoire de base *)
let rec lookup_memory (address: int) (memory: memory): lambda_expr option =
  try Some (List.assoc address memory) with Not_found -> None

let update_memory (address: int) (value: lambda_expr) (memory: memory): memory =
  (address, value) :: List.remove_assoc address memory

let add_to_memory (value: lambda_expr) (memory: memory): (int * memory) =
  let addr = new_memory_address () in
  let updated_memory = update_memory addr value memory in
  (addr, updated_memory)

(* Vérification si une expression est une valeur *)
let rec is_value (expr: lambda_expr): bool =
  match expr with
  | Integer _ -> true
  | Boolean _ -> true
  | UnitValue -> true
  | Abstraction _ -> true
  | LambdaList _ -> true
  | MemoryAddress _ -> true
  | _ -> false

(* Stratégie d'évaluation Call-by-Value *)
let rec eval_step (expr: lambda_expr) (memory: memory): (lambda_expr * memory) option =
  match expr with
  (* Variables *)
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

  (* Opérations arithmétiques *)
  | Addition (Integer a, Integer b) -> Some (Integer (a + b), memory)
  | Subtraction (Integer a, Integer b) -> Some (Integer (a - b), memory)
  | Multiplication (Integer a, Integer b) -> Some (Integer (a * b), memory)

  (* Évaluation logique *)
  | And (Boolean a, Boolean b) -> Some (Boolean (a && b), memory)
  | Or (Boolean a, Boolean b) -> Some (Boolean (a || b), memory)
  | Not (Boolean a) -> Some (Boolean (not a), memory)

  (* Opérations sur listes *)
  | Head (LambdaList (Node (head, _))) -> Some (head, memory)
  | Tail (LambdaList (Node (_, tail))) -> Some (LambdaList tail, memory)
  | Length (LambdaList lst) -> Some (Integer (list_length lst), memory)

  (* Branches conditionnelles *)
  | IfZero (Integer 0, cons, _) -> Some (cons, memory)
  | IfZero (Integer _, _, alt) -> Some (alt, memory)

  | IfBool (Boolean true, cons, _) -> Some (cons, memory)
  | IfBool (Boolean false, _, alt) -> Some (alt, memory)

  (* Assignation et références *)
  | RefValue value when is_value value ->
      let addr, updated_memory = add_to_memory value memory in
      Some (MemoryAddress addr, updated_memory)
  | Deref (MemoryAddress addr) -> (
      match lookup_memory addr memory with
      | Some value -> Some (value, memory)
      | None -> failwith ("Accès mémoire invalide à l'adresse " ^ string_of_int addr)
    )
  | Assign (MemoryAddress addr, value) when is_value value ->
      let updated_memory = update_memory addr value memory in
      Some (UnitValue, updated_memory)

  | _ -> None

(* Fonction pour évaluer jusqu'à obtenir la forme normale *)
let rec eval_to_normal_form (expr: lambda_expr) (memory: memory): (lambda_expr * memory) =
  match eval_step expr memory with
  | Some (next_expr, updated_memory) -> eval_to_normal_form next_expr updated_memory
  | None -> (expr, memory)

(* Fonction auxiliaire pour calculer la longueur d'une liste lambda *)
and list_length lst =
  match lst with
  | Nil -> 0
  | Node (_, tail) -> 1 + list_length tail

