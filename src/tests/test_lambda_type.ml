(* Fichier : test_lambda_type.ml *)
open Lambda_ast
open Lambda_type

(* Fonction de test pour la vérification de typage *)
let test_type_infer (name: string) (expr: lambda_expr) (expected: lambda_type) =
  Printf.printf "\n--- Test : %s ---\n" name;
  Printf.printf "Expression : %s\n" (lambda_expr_to_string expr);
  try
    let inferred = type_infer [] expr in
    Printf.printf "Type inféré : %s\n" (lambda_type_to_string inferred);
    if inferred = expected then
      Printf.printf "✅ Test réussi.\n"
    else
      Printf.printf "❌ Échec. Type attendu : %s\n" (lambda_type_to_string expected)
  with Failure msg ->
    Printf.printf "❌ Échec : %s\n" msg

(* Liste des tests *)
let tests = [
  ("Constante entière", Integer 5, TypeInt);
  ("Addition simple", Addition (Integer 1, Integer 2), TypeInt);
  ("Fonction identité", Abstraction ("x", Variable "x"), TypeArrow (TypeInt, TypeInt));
  ("Application simple",
   Application (Abstraction ("x", Addition (Variable "x", Integer 1)), Integer 5),
   TypeInt);
  ("Liste vide", LambdaList Nil, TypeList TypeInt);
  ("Liste d'entiers", LambdaList (Node (Integer 1, Node (Integer 2, Nil))), TypeList TypeInt);
  ("Head d'une liste", Head (LambdaList (Node (Integer 1, Nil))), TypeInt);
  ("IfZero avec 0",
   IfZero (Integer 0, Integer 1, Integer 2),
   TypeInt);
  ("Référence et assignation",
   Assign (RefValue (Integer 1), Integer 2),
   TypeUnit);
]

(* Fonction principale pour exécuter tous les tests *)
let run_tests () =
  List.iter (fun (name, expr, expected) -> test_type_infer name expr expected) tests

(* Lancer les tests *)
let () = run_tests ()
