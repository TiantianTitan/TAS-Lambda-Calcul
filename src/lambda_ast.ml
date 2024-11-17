(* Définition des structures de données pour les expressions lambda *)

type 'a lambda_list = Nil | Node of 'a * 'a lambda_list
and lambda_expr_list = lambda_expr lambda_list
and lambda_expr = 
  | Variable of string (* Variable *)
  | Application of lambda_expr * lambda_expr (* Application d'une fonction à un argument *)
  | Abstraction of string * lambda_expr (* Fonction lambda : λx. expr *)
  | Integer of int (* Constante entière *)
  | Boolean of bool (* Constante booléenne *)
  | Addition of lambda_expr * lambda_expr (* Addition *)
  | Subtraction of lambda_expr * lambda_expr (* Soustraction *)
  | Multiplication of lambda_expr * lambda_expr (* Multiplication *)
  | LambdaList of lambda_expr_list (* Liste lambda *)
  | Head of lambda_expr (* Récupérer le premier élément d'une liste *)
  | Tail of lambda_expr (* Récupérer la queue d'une liste *)
  | Length of lambda_expr (* Longueur d'une liste *)
  | IfZero of lambda_expr * lambda_expr * lambda_expr (* Condition if x = 0 *)
  | IfNil of lambda_expr * lambda_expr * lambda_expr (* Condition if nil? *)
  | IfBool of lambda_expr * lambda_expr * lambda_expr (* Condition if boolean *)
  | FixPoint of lambda_expr (* Point fixe pour les fonctions récursives *)
  | LetBinding of string * lambda_expr * lambda_expr (* let x = expr1 in expr2 *)
  | UnitValue (* Valeur unité *)
  | RefValue of lambda_expr (* Référence *)
  | MemoryAddress of int (* Adresse mémoire *)
  | Deref of lambda_expr (* Déréférencement *)
  | Assign of lambda_expr * lambda_expr (* Assignation *)
  | And of lambda_expr * lambda_expr (* Opérateur logique AND *)
  | Or of lambda_expr * lambda_expr (* Opérateur logique OR *)
  | Not of lambda_expr (* Opérateur logique NOT *)
  | SumLeft of lambda_expr (* Somme gauche *)
  | SumRight of lambda_expr (* Somme droite *)
  | SumMatch of lambda_expr * string * lambda_expr * lambda_expr (* Match sur somme *)
and memory_binding = int * lambda_expr
and memory = memory_binding list

(* Générer une nouvelle adresse mémoire *)
let new_memory_address () : int =
  let counter = ref 0 in
  counter := !counter + 1;
  !counter

(* Conversion d'une liste lambda en chaîne *)
let rec lambda_list_to_string (lst: lambda_expr_list) : string =
  match lst with
  | Nil -> ""
  | Node (head, Nil) -> lambda_expr_to_string head
  | Node (head, tail) -> lambda_expr_to_string head ^ ", " ^ lambda_list_to_string tail


(* Conversion d'une expression lambda en chaîne *)
and lambda_expr_to_string (expr: lambda_expr) : string =
  match expr with
  | Variable x -> x
  | Boolean b -> string_of_bool b
  | Application (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " " ^ lambda_expr_to_string e2 ^ ")"
  | Abstraction (param, body) -> "λ" ^ param ^ ". " ^ lambda_expr_to_string body
  | Integer n -> string_of_int n
  | Addition (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " + " ^ lambda_expr_to_string e2 ^ ")"
  | Subtraction (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " - " ^ lambda_expr_to_string e2 ^ ")"
  | Multiplication (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " * " ^ lambda_expr_to_string e2 ^ ")"
  | LambdaList lst -> "[" ^ lambda_list_to_string lst ^ "]"
  | Length e -> "length(" ^ lambda_expr_to_string e ^ ")"
  | Head e -> "head(" ^ lambda_expr_to_string e ^ ")"
  | Tail e -> "tail(" ^ lambda_expr_to_string e ^ ")"
  | IfZero (cond, cons, alt) -> "if " ^ lambda_expr_to_string cond ^ " = 0 then " ^ lambda_expr_to_string cons ^ " else " ^ lambda_expr_to_string alt
  | IfNil (cond, cons, alt) -> "if nil?(" ^ lambda_expr_to_string cond ^ ") then " ^ lambda_expr_to_string cons ^ " else " ^ lambda_expr_to_string alt
  | IfBool (cond, cons, alt) -> "if " ^ lambda_expr_to_string cond ^ " then " ^ lambda_expr_to_string cons ^ " else " ^ lambda_expr_to_string alt
  | FixPoint f -> "fix(" ^ lambda_expr_to_string f ^ ")"
  | LetBinding (var, value, body) -> "let " ^ var ^ " = " ^ lambda_expr_to_string value ^ " in " ^ lambda_expr_to_string body
  | UnitValue -> "()"
  | RefValue e -> "ref(" ^ lambda_expr_to_string e ^ ")"
  | MemoryAddress addr -> "addr(" ^ string_of_int addr ^ ")"
  | Deref e -> "!" ^ lambda_expr_to_string e
  | Assign (e1, e2) -> lambda_expr_to_string e1 ^ " := " ^ lambda_expr_to_string e2
  | And (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " && " ^ lambda_expr_to_string e2 ^ ")"
  | Or (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " || " ^ lambda_expr_to_string e2 ^ ")"
  | Not e -> "not(" ^ lambda_expr_to_string e ^ ")"
  | SumLeft e -> "Left(" ^ lambda_expr_to_string e ^ ")"
  | SumRight e -> "Right(" ^ lambda_expr_to_string e ^ ")"
  | SumMatch (e, x, left, right) -> 
      "match " ^ lambda_expr_to_string e ^ " with Left(" ^ x ^ ") -> " ^ lambda_expr_to_string left ^ " | Right(" ^ x ^ ") -> " ^ lambda_expr_to_string right

(* Fonction d'affichage pour les expressions lambda *)
let print_lambda_expr expr =
  Printf.printf "%s\n" (lambda_expr_to_string expr)

(* Remplacement des variables par des valeurs *)
let rec substitute (var: string) (value: lambda_expr) (expr: lambda_expr): lambda_expr =
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
  | Length e -> Length (substitute var value e)
  | IfZero (cond, cons, alt) -> IfZero (substitute var value cond, substitute var value cons, substitute var value alt)
  | IfNil (cond, cons, alt) -> IfNil (substitute var value cond, substitute var value cons, substitute var value alt)
  | IfBool (cond, cons, alt) -> IfBool (substitute var value cond, substitute var value cons, substitute var value alt)
  | FixPoint e -> FixPoint (substitute var value e)
  | LetBinding (x, e1, e2) when x <> var ->
      LetBinding (x, substitute var value e1, substitute var value e2)
  | RefValue e -> RefValue (substitute var value e)
  | Deref e -> Deref (substitute var value e)
  | Assign (e1, e2) -> Assign (substitute var value e1, substitute var value e2)
  | And (e1, e2) -> And (substitute var value e1, substitute var value e2)
  | Or (e1, e2) -> Or (substitute var value e1, substitute var value e2)
  | Not e -> Not (substitute var value e)
  | _ -> expr

and substitute_in_list (var: string) (value: lambda_expr) (lst: lambda_expr_list): lambda_expr_list =
  match lst with
  | Nil -> Nil
  | Node (head, tail) -> Node (substitute var value head, substitute_in_list var value tail)
