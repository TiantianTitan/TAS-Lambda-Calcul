(* lambda_eval.ml *)


type 'a lambda_list = Nil | Node of 'a * 'a lambda_list
and lambda_expr_list = lambda_expr lambda_list
and lambda_expr = 
  | Variable of string 
  | Application of lambda_expr * lambda_expr  
  | Abstraction of string * lambda_expr
  | Integer of int 
  | Addition of lambda_expr * lambda_expr 
  | Subtraction of lambda_expr * lambda_expr 
  | Multiplication of lambda_expr * lambda_expr 
  | LambdaList of lambda_expr_list  
  | Head of lambda_expr  
  | Tail of lambda_expr  
  | IfZero of lambda_expr * lambda_expr * lambda_expr  
  | IfNil of lambda_expr * lambda_expr * lambda_expr  
  | FixPoint of lambda_expr
  | LetBinding of string * lambda_expr * lambda_expr
  | UnitValue 
  | RefValue of lambda_expr
  | MemoryAddress of int
  | Deref of lambda_expr
  | Assign of lambda_expr * lambda_expr
  | SumLeft of lambda_expr 
  | SumRight of lambda_expr 
  | SumMatch of lambda_expr * string * lambda_expr * lambda_expr

  (* extension - boolean *)
  | Boolean of bool
  | And of lambda_expr * lambda_expr
  | Or of lambda_expr * lambda_expr
  | Not of lambda_expr

  (* extension - List *)
  | Length of lambda_expr
  | Map of lambda_expr * lambda_expr
  | Filter of lambda_expr * lambda_expr

  (* extension - Match *)
  | Match of lambda_expr * (lambda_pattern * lambda_expr) list
  and lambda_pattern =
  | PInt of int
  | PBool of bool
  | PVar of string
  | PWildcard

and memory_binding = int * lambda_expr
and memory = memory_binding list

let new_memory_address () : int =
  let counter = ref 0 in
  counter := !counter + 1;
  !counter

let rec lambda_list_to_string (lst: lambda_expr_list) : string =
  match lst with
  | Nil -> "[]"
  | Node (head, Nil) -> lambda_expr_to_string head
  | Node (head, tail) -> lambda_expr_to_string head ^ ", " ^ lambda_list_to_string tail

and lambda_expr_to_string (expr: lambda_expr) : string =
  match expr with
  | Variable x -> x
  | Application (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " " ^ lambda_expr_to_string e2 ^ ")"
  | Abstraction (param, body) -> "λ" ^ param ^ ". " ^ lambda_expr_to_string body
  | Integer n -> string_of_int n
  | Addition (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " + " ^ lambda_expr_to_string e2 ^ ")"
  | Subtraction (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " - " ^ lambda_expr_to_string e2 ^ ")"
  | Multiplication (e1, e2) -> "(" ^ lambda_expr_to_string e1 ^ " * " ^ lambda_expr_to_string e2 ^ ")"
  | LambdaList lst -> "[" ^ lambda_list_to_string lst ^ "]"
  | Head e -> "head(" ^ lambda_expr_to_string e ^ ")"
  | Tail e -> "tail(" ^ lambda_expr_to_string e ^ ")"
  | IfZero (cond, cons, alt) -> "if " ^ lambda_expr_to_string cond ^ " = 0 then " ^ lambda_expr_to_string cons ^ " else " ^ lambda_expr_to_string alt
  | IfNil (cond, cons, alt) -> "if nil?(" ^ lambda_expr_to_string cond ^ ") then " ^ lambda_expr_to_string cons ^ " else " ^ lambda_expr_to_string alt
  | FixPoint f -> "fix(" ^ lambda_expr_to_string f ^ ")"
  | LetBinding (var, value, body) -> "let " ^ var ^ " = " ^ lambda_expr_to_string value ^ " in " ^ lambda_expr_to_string body
  | UnitValue -> "()"
  | RefValue e -> "ref(" ^ lambda_expr_to_string e ^ ")"
  | MemoryAddress addr -> "addr(" ^ string_of_int addr ^ ")"
  | Deref e -> "!" ^ lambda_expr_to_string e
  | Assign (e1, e2) -> lambda_expr_to_string e1 ^ " := " ^ lambda_expr_to_string e2
  | SumLeft e -> "Left(" ^ lambda_expr_to_string e ^ ")"
  | SumRight e -> "Right(" ^ lambda_expr_to_string e ^ ")"
  | SumMatch (e, x, left, right) -> 
      "match " ^ lambda_expr_to_string e ^ " with Left(" ^ x ^ ") -> " ^ lambda_expr_to_string left ^ " | Right(" ^ x ^ ") -> " ^ lambda_expr_to_string right

let print_lambda_expr expr =
  Printf.printf "%s\n" (lambda_expr_to_string expr)


let rec substitute (var: string) (value: lambda_expr) (expr: lambda_expr): lambda_expr =
  match expr with
  | Variable x when x = var -> value
  | Variable _ -> expr
  | Application (e1, e2) -> Application (substitute var value e1, substitute var value e2)
  | Abstraction (param, body) when param <> var -> Abstraction (param, substitute var value body)
  | Addition (e1, e2) -> Addition (substitute var value e1, substitute var value e2)
  | Subtraction (e1, e2) -> Subtraction (substitute var value e1, substitute var value e2)
  | Multiplication (e1, e2) -> Multiplication (substitute var value e1, substitute var value e2)
  | _ -> expr  


  