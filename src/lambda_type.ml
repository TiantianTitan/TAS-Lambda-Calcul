open Lambda_ast

(* Définition des types pour les expressions lambda *)
type lambda_type =
  | TypeInt
  | TypeBool
  | TypeArrow of lambda_type * lambda_type
  | TypeList of lambda_type
  | TypeUnit
  | TypeRef of lambda_type

(* Environnement de typage *)
type type_env = (string * lambda_type) list

(* Conversion des types en chaîne *)
let rec lambda_type_to_string = function
  | TypeInt -> "Int"
  | TypeBool -> "Bool"
  | TypeArrow (t1, t2) -> "(" ^ lambda_type_to_string t1 ^ " -> " ^ lambda_type_to_string t2 ^ ")"
  | TypeList t -> "[" ^ lambda_type_to_string t ^ "]"
  | TypeUnit -> "Unit"
  | TypeRef t -> "Ref(" ^ lambda_type_to_string t ^ ")"

(* Recherche d'un type dans l'environnement *)
let rec lookup_type (x: string) (env: type_env) : lambda_type =
  match env with
  | [] -> failwith ("Variable non trouvée dans l'environnement : " ^ x)
  | (y, t) :: rest -> if x = y then t else lookup_type x rest

(* Inférence des types *)
let rec type_infer (env: type_env) (expr: lambda_expr) : lambda_type =
  match expr with
  | Integer _ -> TypeInt
  | Boolean _ -> TypeBool
  | Variable x -> lookup_type x env
  | Addition (e1, e2) | Subtraction (e1, e2) | Multiplication (e1, e2) ->
      if type_infer env e1 = TypeInt && type_infer env e2 = TypeInt then TypeInt
      else failwith "Opération arithmétique sur des types non entiers"
  | And (e1, e2) | Or (e1, e2) ->
      if type_infer env e1 = TypeBool && type_infer env e2 = TypeBool then TypeBool
      else failwith "Opération logique sur des types non booléens"
  | Not e ->
      if type_infer env e = TypeBool then TypeBool
      else failwith "Opération NOT sur un type non booléen"
  | Abstraction (x, body) ->
      let arg_type = TypeInt in
      let new_env = (x, arg_type) :: env in
      let body_type = type_infer new_env body in
      TypeArrow (arg_type, body_type)
  | Application (e1, e2) -> (
      match type_infer env e1 with
      | TypeArrow (arg_type, return_type) ->
          if type_infer env e2 = arg_type then return_type
          else failwith "Type d'argument incompatible"
      | _ -> failwith "Application à un type non fonctionnel"
    )
  | LambdaList Nil -> TypeList TypeInt
  | LambdaList (Node (head, _)) ->
      TypeList (type_infer env head)
  | Head lst | Tail lst ->
      (match type_infer env lst with
      | TypeList t -> t
      | _ -> failwith "Opération sur un type non liste")
  | Length lst ->
      (match type_infer env lst with
      | TypeList _ -> TypeInt
      | _ -> failwith "Longueur appelée sur un type non liste")
  | IfZero (cond, then_branch, else_branch) ->
      if type_infer env cond = TypeInt then
        let then_type = type_infer env then_branch in
        let else_type = type_infer env else_branch in
        if then_type = else_type then then_type
        else failwith "Branches de IfZero de types incompatibles"
      else failwith "Condition de IfZero de type non entier"
  | IfBool (cond, then_branch, else_branch) ->
      if type_infer env cond = TypeBool then
        let then_type = type_infer env then_branch in
        let else_type = type_infer env else_branch in
        if then_type = else_type then then_type
        else failwith "Branches de IfBool de types incompatibles"
      else failwith "Condition de IfBool de type non booléen"
  | LetBinding (x, value, body) ->
      let value_type = type_infer env value in
      let new_env = (x, value_type) :: env in
      type_infer new_env body
  | _ -> failwith "Expression non prise en charge"
