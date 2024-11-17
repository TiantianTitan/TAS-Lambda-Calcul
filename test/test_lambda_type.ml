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
  (* Constantes et opérations arithmétiques *)
  ("Constante entière", Integer 5, TypeInt);
  ("Addition simple", Addition (Integer 2, Integer 3), TypeInt);

  (* Booléens et opérations logiques *)
  ("Valeur booléenne true", Boolean true, TypeBool);
  ("Valeur booléenne false", Boolean false, TypeBool);
  ("Opération AND", And (Boolean true, Boolean false), TypeBool);
  ("Opération OR", Or (Boolean false, Boolean true), TypeBool);
  ("Opération NOT", Not (Boolean true), TypeBool);

  (* Listes *)
  ("Liste vide", LambdaList Nil, TypeList TypeInt);
  ("Liste d'entiers", LambdaList (Node (Integer 1, Node (Integer 2, Nil))), TypeList TypeInt);
  ("Longueur d'une liste", Length (LambdaList (Node (Integer 1, Node (Integer 2, Nil)))), TypeInt);
  ("Head d'une liste", Head (LambdaList (Node (Integer 1, Nil))), TypeInt);

  (* Map et Filter *)
  ("Map sur une liste",
   Map (Abstraction ("x", Addition (Variable "x", Integer 1)),
        LambdaList (Node (Integer 1, Node (Integer 2, Nil)))),
   TypeList TypeInt);
  ("Filter sur une liste",
   Filter (Abstraction ("x", IfZero (Subtraction (Variable "x", Integer 2), Boolean true, Boolean false)),
           LambdaList (Node (Integer 1, Node (Integer 2, Node (Integer 3, Nil))))),
   TypeList TypeInt);

  (* Boucles While et For *)
  ("Boucle While",
   While (Not (Boolean false), UnitValue),
   TypeUnit);
  ("Boucle For",
   For ("i", Integer 1, Integer 3, UnitValue),
   TypeUnit);

  (* Conditionnelles *)
  ("IfZero vrai", IfZero (Integer 0, Integer 1, Integer 2), TypeInt);
  ("IfZero faux", IfZero (Integer 1, Integer 1, Integer 2), TypeInt);

  (* Références et assignations *)
  ("Création de référence", RefValue (Integer 5), TypeRef TypeInt);
  ("Assignation de référence", Assign (RefValue (Integer 1), Integer 2), TypeUnit);
]

(* Fonction principale pour exécuter tous les tests *)
let run_tests () =
  List.iter (fun (name, expr, expected) -> test_type_infer name expr expected) tests

(* Lancer les tests *)
let () = run_tests ()
