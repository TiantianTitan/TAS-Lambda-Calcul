open Ast

let rec print_term (t: pterm): string =
  match t with
  Var x -> x
  | App (t1,t2) ->  "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun" ^ x ^ " -> " ^ (print_term t) ^ ")"