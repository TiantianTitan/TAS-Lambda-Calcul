open Lambda_ast
open Lambda_eval

(* Fonction de test pour l'évaluation *)
let test_eval (name: string) (expr: lambda_expr) (expected: lambda_expr) =
  Printf.printf "\n--- Test : %s ---\n" name;
  Printf.printf "Expression : %s\n" (lambda_expr_to_string expr);
  try
    let result, _ = eval_to_normal_form expr [] in
    Printf.printf "Résultat : %s\n" (lambda_expr_to_string result);
    if result = expected then
      Printf.printf "✅ Test réussi.\n"
    else
      Printf.printf "❌ Échec. Résultat attendu : %s\n" (lambda_expr_to_string expected)
  with Failure msg ->
    Printf.printf "❌ Échec : %s\n" msg

(* Liste des tests *)
let tests = [
  (* Constantes et opérations arithmétiques *)
  ("Constante entière", Integer 5, Integer 5);
  ("Addition simple", Addition (Integer 2, Integer 3), Integer 5);
  ("Soustraction simple", Subtraction (Integer 5, Integer 3), Integer 2);
  ("Multiplication simple", Multiplication (Integer 2, Integer 4), Integer 8);

  (* Booléens et opérations logiques *)
  ("Valeur booléenne true", Boolean true, Boolean true);
  ("Valeur booléenne false", Boolean false, Boolean false);
  ("Opération AND", And (Boolean true, Boolean false), Boolean false);
  ("Opération OR", Or (Boolean false, Boolean true), Boolean true);
  ("Opération NOT", Not (Boolean true), Boolean false);

  (* Listes *)
  ("Liste vide", LambdaList Nil, LambdaList Nil);
  ("Liste non vide", LambdaList (Node (Integer 1, Node (Integer 2, Nil))), LambdaList (Node (Integer 1, Node (Integer 2, Nil))));
  ("Longueur d'une liste", Length (LambdaList (Node (Integer 1, Node (Integer 2, Nil)))), Integer 2);
  ("Head d'une liste", Head (LambdaList (Node (Integer 1, Nil))), Integer 1);
  ("Tail d'une liste", Tail (LambdaList (Node (Integer 1, Node (Integer 2, Nil)))), LambdaList (Node (Integer 2, Nil)));

  (* Map et Filter *)
  ("Map sur une liste",
   Map (Abstraction ("x", Addition (Variable "x", Integer 1)),
        LambdaList (Node (Integer 1, Node (Integer 2, Nil)))),
   LambdaList (Node (Integer 2, Node (Integer 3, Nil))));
  ("Filter sur une liste",
   Filter (Abstraction ("x", IfZero (Subtraction (Variable "x", Integer 2), Boolean true, Boolean false)),
           LambdaList (Node (Integer 1, Node (Integer 2, Node (Integer 3, Nil))))),
   LambdaList (Node (Integer 2, Nil)));

  (* Conditionnelles *)
  ("IfZero vrai", IfZero (Integer 0, Integer 1, Integer 2), Integer 1);
  ("IfZero faux", IfZero (Integer 1, Integer 1, Integer 2), Integer 2);

  (* Références et assignations *)
  ("Création de référence", RefValue (Integer 5), MemoryAddress 1);
  ("Assignation de référence",
   Assign (RefValue (Integer 1), Integer 2), UnitValue);

  (* Boucles While et For *)
  ("Boucle While simple",
   While (Not (Variable "x"), Assign (Variable "x", Integer 1)), UnitValue);
  ("Boucle For simple",
   For ("i", Integer 1, Integer 3, UnitValue), UnitValue);

]

(* Fonction principale pour exécuter tous les tests *)
let run_tests () =
  List.iter (fun (name, expr, expected) -> test_eval name expr expected) tests

(* Lancer les tests *)
let () = run_tests ()
