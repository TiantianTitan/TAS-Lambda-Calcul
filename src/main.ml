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
    with _ ->
      failwith "Erreur de syntaxe dans if-then-else"
  else if String.starts_with ~prefix:"λ" line || String.starts_with ~prefix:"\\" line then
    let regex = Str.regexp "\\(λ\\|\\\\\\)\\([a-zA-Z0-9]+\\)\\.\\(.*\\)" in
    if Str.string_match regex line 0 then
      let param = Str.matched_group 2 line in
      let body = Str.matched_group 3 line in
      Abstraction (param, parse_lambda_expr body)
    else
      failwith "Erreur de syntaxe dans lambda"
  else if String.starts_with ~prefix:"(" line && String.ends_with ~suffix:")" line then
    let inside = String.sub line 1 (String.length line - 2) in
    parse_lambda_expr inside
  else if String.starts_with ~prefix:"let" line then
    let regex = Str.regexp "let \\([a-zA-Z]+\\) = \\(.*\\) in \\(.*\\)" in
    if Str.string_match regex line 0 then
      let var = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let body = Str.matched_group 3 line in
      LetBinding (var, parse_lambda_expr value, parse_lambda_expr body)
    else
      failwith "Erreur de syntaxe dans let-in"
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
