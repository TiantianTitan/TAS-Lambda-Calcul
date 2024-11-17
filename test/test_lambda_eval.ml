open Lambda_ast
open Lambda_eval

(* Liste des tests *)
let test_expressions = [
  ("Addition simple", Addition (Integer 1, Integer 2), Integer 3);
  ("Multiplication simple", Multiplication (Integer 3, Integer 4), Integer 12);
  ("Application", Application (Abstraction ("x", Addition (Variable "x", Integer 1)), Integer 5), Integer 6);
  ("IfZero vrai", IfZero (Integer 0, Integer 42, Integer 24), Integer 42);
  ("IfZero faux", IfZero (Integer 5, Integer 42, Integer 24), Integer 24);
]

(* Fonction pour exécuter un test *)
let run_test (name, expr, expected) =
  let result, _ = eval_to_normal_form expr [] in
  if result = expected then
    Printf.printf "✅ Test réussi : %s\n" name
  else
    Printf.printf "❌ Échec : %s\n  Attendu : %s\n  Obtenu : %s\n"
      name
      (lambda_expr_to_string expected)
      (lambda_expr_to_string result)

(* Fonction principale de test *)
let () =
  List.iter run_test test_expressions
