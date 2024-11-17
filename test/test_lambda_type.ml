open Lambda_ast
open Lambda_type

(* Liste des tests *)
let test_types = [
  ("Constante entière", Integer 42, TypeInt);
  ("Abstraction identité", Abstraction ("x", Variable "x"), TypeArrow (TypeInt, TypeInt));
  ("Application simple", Application (Abstraction ("x", Variable "x"), Integer 5), TypeInt);
  ("Addition", Addition (Integer 1, Integer 2), TypeInt);
  ("IfZero vrai", IfZero (Integer 0, Integer 1, Integer 2), TypeInt);
]

(* Fonction pour exécuter un test *)
let run_test (name, expr, expected_type) =
  try
    let inferred_type = type_infer [] expr in
    if inferred_type = expected_type then
      Printf.printf "✅ Test réussi : %s\n" name
    else
      Printf.printf "❌ Échec : %s\n  Attendu : %s\n  Obtenu : %s\n"
        name
        (lambda_type_to_string expected_type)
        (lambda_type_to_string inferred_type)
  with Failure msg ->
    Printf.printf "❌ Échec : %s\n  Erreur : %s\n" name msg

(* Fonction principale de test *)
let () =
  List.iter run_test test_types
