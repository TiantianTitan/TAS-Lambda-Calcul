(* Fichier : lambda_type.ml *)
open Lambda_ast

(* Définition des types pour la vérification de typage *)
type lambda_type =
  | TypeInt
  | TypeArrow of lambda_type * lambda_type
  | TypeList of lambda_type
  | TypeUnit
  | TypeRef of lambda_type

(* Environnement de typage *)
type type_env = (string * lambda_type) list

(* Conversion d'un type en chaîne de caractères *)
let rec lambda_type_to_string = function
  | TypeInt -> "Int"
  | TypeArrow (t1, t2) -> "(" ^ lambda_type_to_string t1 ^ " -> " ^ lambda_type_to_string t2 ^ ")"
  | TypeList t -> "[" ^ lambda_type_to_string t ^ "]"
  | TypeUnit -> "Unit"
  | TypeRef t -> "Ref(" ^ lambda_type_to_string t ^ ")"

(* Fonction pour rechercher le type d'une variable dans l'environnement *)
let rec lookup_type (x: string) (env: type_env) : lambda_type =
  match env with
  | [] -> failwith ("Variable non trouvée dans l'environnement : " ^ x)
  | (y, t) :: rest -> if x = y then t else lookup_type x rest

(* Fonction principale de vérification de typage *)
let rec type_infer (env: type_env) (expr: lambda_expr) : lambda_type =
  match expr with
  | Integer _ -> TypeInt
  | Variable x -> lookup_type x env

  (* extension - boolean *)
  | Boolean _ -> TypeBool
  | And (e1, e2) | Or (e1, e2) ->
    let t1 = type_infer env e1 in
    let t2 = type_infer env e2 in
    if t1 = TypeBool && t2 = TypeBool then TypeBool
    else failwith "Opération logique sur des types non booléens"
  | Not e ->
    let t = type_infer env e in
    if t = TypeBool then TypeBool
    else failwith "Not appliqué sur un type non booléen"

  | Addition (e1, e2) | Subtraction (e1, e2) | Multiplication (e1, e2) ->
      let t1 = type_infer env e1 in
      let t2 = type_infer env e2 in
      if t1 = TypeInt && t2 = TypeInt then TypeInt
      else failwith "Opération arithmétique sur des types non entiers"
      | Abstraction (x, body) ->
        let arg_type = TypeInt in
        let new_env = (x, arg_type) :: env in
        let body_type = type_infer new_env body in
        TypeArrow (arg_type, body_type)
  | Application (e1, e2) ->
      let t1 = type_infer env e1 in
      let t2 = type_infer env e2 in
      (match t1 with
      | TypeArrow (arg_type, return_type) ->
          if t2 = arg_type then return_type
          else failwith "Type de l'argument non compatible avec la fonction"
      | _ -> failwith "Application sur un type non fonctionnel")
  | LambdaList elements ->
      (match elements with
      | Nil -> TypeList TypeInt (* Liste vide, supposée de type int par défaut *)
      | Node (head, tail) ->
          let head_type = type_infer env head in
          let tail_type = type_infer env (LambdaList tail) in
          (match tail_type with
          | TypeList t when t = head_type -> TypeList t
          | _ -> failwith "Incohérence des types dans la liste"))
  | Head lst ->
      (match type_infer env lst with
      | TypeList t -> t
      | _ -> failwith "Head appelé sur un type non liste")
  | Tail lst ->
      (match type_infer env lst with
      | TypeList t -> TypeList t
      | _ -> failwith "Tail appelé sur un type non liste")
  | IfZero (cond, then_branch, else_branch) ->
      let cond_type = type_infer env cond in
      let then_type = type_infer env then_branch in
      let else_type = type_infer env else_branch in
      if cond_type = TypeInt && then_type = else_type then then_type
      else failwith "Condition IfZero incorrecte"
  | IfNil (cond, cons, alt) ->
      let cond_type = type_infer env cond in
      let cons_type = type_infer env cons in
      let alt_type = type_infer env alt in
      (match cond_type with
      | TypeList _ when cons_type = alt_type -> cons_type
      | _ -> failwith "Condition IfNil incorrecte")

      | FixPoint f ->
        (* 推导 `f` 的类型 *)
        let t = type_infer env f in
        Printf.printf "FixPoint 输入类型：%s\n" (lambda_type_to_string t);
        (match t with
        | TypeArrow (arg_type, return_type) ->
            if arg_type = return_type then return_type
            else failwith ("FixPoint 类型不匹配：参数类型和返回类型必须一致。\n"
                           ^ "参数类型：" ^ lambda_type_to_string arg_type
                           ^ "，返回类型：" ^ lambda_type_to_string return_type)
        | _ -> failwith "FixPoint 必须是函数类型 (T -> T)")
    
    
    
    
    
  | LetBinding (x, value, body) ->
      let value_type = type_infer env value in
      let new_env = (x, value_type) :: env in
      type_infer new_env body
  | RefValue e ->
      let t = type_infer env e in
      TypeRef t
  | Deref e ->
      (match type_infer env e with
      | TypeRef t -> t
      | _ -> failwith "Déréférencement sur un type non référence")
  | Assign (e1, e2) ->
      (match type_infer env e1 with
      | TypeRef t1 ->
          let t2 = type_infer env e2 in
          if t1 = t2 then TypeUnit
          else failwith "Assignation de types incompatibles"
      | _ -> failwith "Assignation sur un type non référence")
  | UnitValue -> TypeUnit
  | MemoryAddress _ -> failwith "Les adresses mémoire ne peuvent pas être directement typées"
  | SumLeft e ->
      let t = type_infer env e in
      TypeArrow (t, TypeInt)
  | SumRight e ->
      let t = type_infer env e in
      TypeArrow (TypeInt, t)
  | SumMatch (e, _, left, right) ->
      let e_type = type_infer env e in
      let left_type = type_infer env left in
      let right_type = type_infer env right in
      if e_type = left_type && e_type = right_type then e_type
      else failwith "SumMatch branches have incompatible types"


      

(* Fonction pour inférer et afficher le type d'une expression *)
let infer_and_print (env: type_env) (expr: lambda_expr) =
  try
    let t = type_infer env expr in
    Printf.printf "Expression : %s\n" (lambda_expr_to_string expr);
    Printf.printf "Type : %s\n" (lambda_type_to_string t)
  with Failure msg ->
    Printf.printf "Erreur : %s\n" msg
