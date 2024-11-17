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
  else if String.starts_with ~prefix:"head" line then
    let regex = Str.regexp "head(\\(.*\\))" in
    if Str.string_match regex line 0 then
      let list_expr = Str.matched_group 1 line in
      Head (parse_lambda_expr list_expr)
    else
      failwith "Erreur de syntaxe dans head"
  else if String.starts_with ~prefix:"tail" line then
    let regex = Str.regexp "tail(\\(.*\\))" in
    if Str.string_match regex line 0 then
      let list_expr = Str.matched_group 1 line in
      Tail (parse_lambda_expr list_expr)
    else
      failwith "Erreur de syntaxe dans tail"
  else if String.starts_with ~prefix:"length" line then
    let regex = Str.regexp "length(\\(.*\\))" in
    if Str.string_match regex line 0 then
      let list_expr = Str.matched_group 1 line in
      Length (parse_lambda_expr list_expr)
    else
      failwith "Erreur de syntaxe dans length"
  else if String.starts_with ~prefix:"[" line && String.ends_with ~suffix:"]" line then
    let inside = String.sub line 1 (String.length line - 2) in
    let rec parse_list_elements (s: string) =
      let buffer = Buffer.create 16 in
      let rec aux i depth acc =
        if i >= String.length s then
          if Buffer.length buffer > 0 then
            acc @ [Buffer.contents buffer |> String.trim]
          else acc
        else
          let c = s.[i] in
          match c with
          | '[' -> Buffer.add_char buffer c; aux (i + 1) (depth + 1) acc
          | ']' -> Buffer.add_char buffer c; aux (i + 1) (depth - 1) acc
          | ',' when depth = 0 ->
              acc @ [Buffer.contents buffer |> String.trim]
              |> fun acc' -> Buffer.clear buffer; aux (i + 1) depth acc'
          | _ -> Buffer.add_char buffer c; aux (i + 1) depth acc
      in
      aux 0 0 []
    in
    let elements = parse_list_elements inside in
    let rec parse_list = function
      | [] -> Nil
      | h :: t -> Node (parse_lambda_expr h, parse_list t)
    in
    LambdaList (parse_list elements)
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

(* Fonction REPL : boucle interactive *)
let rec repl memory =
  Printf.printf "> ";
  match read_line () with
  | "exit" -> Printf.printf "Au revoir !\n"; exit 0
  | line ->
      (try
         let expr = parse_lambda_expr line in
         Printf.printf "Expression : %s\n" (lambda_expr_to_string expr);
         let inferred_type = type_infer [] expr in
         Printf.printf "Type : %s\n" (lambda_type_to_string inferred_type);
         let result, updated_memory = eval_to_normal_form expr memory in
         Printf.printf "Résultat : %s\n" (lambda_expr_to_string result);
         repl updated_memory
       with Failure msg ->
         Printf.printf "Erreur : %s\n" msg;
         repl memory)

(* Démarrage du programme *)
let () = repl []
