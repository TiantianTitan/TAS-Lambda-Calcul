open Lambda_ast
open Lambda_eval
open Lambda_type

(* Fonction pour analyser une ligne en une expression lambda *)
let rec parse_lambda_expr (line: string): lambda_expr =
  let line = String.trim line in
  if String.starts_with ~prefix:"if" line then
    try
      let regex = Str.regexp "if \\(.*\\) = 0 then \\(.*\\) else \\(.*\\)" in
      if Str.string_match regex line 0 then
        let cond = Str.matched_group 1 line in
        let then_branch = Str.matched_group 2 line in
        let else_branch = Str.matched_group 3 line in
        IfZero (parse_lambda_expr cond, parse_lambda_expr then_branch, parse_lambda_expr else_branch)
      else
        failwith "Erreur de syntaxe dans if-then-else"
    with _ -> failwith "Erreur de syntaxe dans if-then-else"
  (* Parse Boolean values *)
  else if line = "true" then Boolean true
  else if line = "false" then Boolean false
  (* Parse Boolean operations *)
  else if String.contains line '&' then
    let parts = String.split_on_char '&' line in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans l'opération AND"
    else
      let left = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let right = parse_lambda_expr (String.trim (List.nth parts 1)) in
      And (left, right)
  else if String.contains line '|' then
    let parts = String.split_on_char '|' line in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans l'opération OR"
    else
      let left = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let right = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Or (left, right)
  else if String.starts_with ~prefix:"not" line then
    let expr = String.sub line 3 (String.length line - 3) |> String.trim in
    Not (parse_lambda_expr expr)
  (* Parse length operation *)
  else if String.starts_with ~prefix:"length(" line && String.ends_with ~suffix:")" line then
    let inside = String.sub line 7 (String.length line - 8) in
    Length (parse_lambda_expr inside)
  (* Parse map operation *)
  else if String.starts_with ~prefix:"map(" line && String.ends_with ~suffix:")" line then
    let inside = String.sub line 4 (String.length line - 5) in
    let parts = String.split_on_char ',' inside in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans map"
    else
      let func = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let lst = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Map (func, lst)
  (* Parse filter operation *)
  else if String.starts_with ~prefix:"filter(" line && String.ends_with ~suffix:")" line then
    let inside = String.sub line 7 (String.length line - 8) in
    let parts = String.split_on_char ',' inside in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans filter"
    else
      let func = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let lst = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Filter (func, lst)
  (* Parse let-in *)
  else if String.starts_with ~prefix:"let" line then
    let regex = Str.regexp "let \\([a-zA-Z]+\\) = \\(.*\\) in \\(.*\\)" in
    if Str.string_match regex line 0 then
      let var = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let body = Str.matched_group 3 line in
      LetBinding (var, parse_lambda_expr value, parse_lambda_expr body)
    else
      failwith "Erreur de syntaxe dans let-in"
  (* Parse arithmetic and variable operations *)
  else if String.contains line '+' then
    let parts = String.split_on_char '+' line in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans l'addition"
    else
      let left = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let right = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Addition (left, right)
  else if String.contains line '-' then
    let parts = String.split_on_char '-' line in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans la soustraction"
    else
      let left = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let right = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Subtraction (left, right)
  else if String.contains line '*' then
    let parts = String.split_on_char '*' line in
    if List.length parts <> 2 then failwith "Erreur de syntaxe dans la multiplication"
    else
      let left = parse_lambda_expr (String.trim (List.nth parts 0)) in
      let right = parse_lambda_expr (String.trim (List.nth parts 1)) in
      Multiplication (left, right)
  (* Parse numbers and variables *)
  else if String.for_all (fun c -> c >= '0' && c <= '9') (String.trim line) then
    Integer (int_of_string (String.trim line))
  else if String.for_all (fun c -> c >= 'a' && c <= 'z') (String.trim line) then
    Variable (String.trim line)
  else
    failwith ("Erreur de syntaxe : " ^ line)

(* Évaluation d'une expression en forme normale *)
let rec eval_to_normal_form (expr: lambda_expr) (memory: memory): (lambda_expr * memory) =
  Printf.printf "Évaluation : %s\n" (lambda_expr_to_string expr);
  match eval_step expr memory with
  | Some (next_expr, updated_memory) ->
      Printf.printf "Étape intermédiaire : %s\n" (lambda_expr_to_string next_expr);
      eval_to_normal_form next_expr updated_memory
  | None -> (expr, memory)

(* Fonction REPL : Read-Eval-Print Loop *)
let rec repl memory =
  Printf.printf "> ";
  match read_line () with
  | "exit" -> Printf.printf "Au revoir !\n"; exit 0
  | line ->
      (try
         let expr = parse_lambda_expr line in
         Printf.printf "Expression : %s\n" (lambda_expr_to_string expr);
         let result, updated_memory = eval_to_normal_form expr memory in
         Printf.printf "Résultat : %s\n" (lambda_expr_to_string result);
         repl updated_memory
       with Failure msg ->
         Printf.printf "Erreur : %s\n" msg;
         repl memory)

let () = repl []
