open Core
open Exns

module Ident = struct
  module T = struct
    type t = Ident of string [@@deriving compare, sexp]

    let pp fmt (Ident x) = Format.fprintf fmt "%s" x
    let show (Ident x) = x
  end

  include T
  include Comparable.Make (T)
end

type type_ref =
  | IntType
  | FloatType
  | StringType
  | BoolType
  | ListType of type_ref
  | OptionType of type_ref

let rec show_type_ref = function
  | IntType -> "Int"
  | FloatType -> "Real"
  | StringType -> "String"
  | BoolType -> "Bool"
  | ListType t -> Format.sprintf "List_%s" (show_type_ref t)
  | OptionType t -> Format.sprintf "Optional[%s]" (show_type_ref t)

let pp_type_ref fmt t = Format.fprintf fmt "%s" (show_type_ref t)

type fun_name = Fold | Filter | Insert | Other of Ident.t
[@@deriving show {with_path = false}]

type expr =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Var of Ident.t
  | Null of type_ref option
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | Ge of expr * expr
  | Gt of expr * expr
  | Le of expr * expr
  | Lt of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Schema of type_ref list
  | Tuple of expr list
  | List of expr list * type_ref option
  | Proj of expr * int
  | Tail of expr
  | If of expr * expr * expr
  | Match of expr * (expr * expr) list
  | Lambda of Ident.t list * expr
  | Fix of Ident.t * Ident.t list * expr
  | App of fun_name * expr list
[@@deriving show {with_path = false}]

type stmt = Decl of Ident.t * type_ref list option * expr
[@@deriving show {with_path = false}]

type prog = stmt list [@@deriving show {with_path = false}]

let rec size_of_type_ref = function
  | ListType t | OptionType t -> 1 + size_of_type_ref t
  | _ -> 1

let rec size_of_expr = function
  | Bool _ | Int _ | Float _ | String _ | Var _ -> 1
  | Null t -> 1 + Option.value_map t ~default:0 ~f:size_of_type_ref
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2)
  | Eq (e1, e2)
  | Ge (e1, e2)
  | Gt (e1, e2)
  | Le (e1, e2)
  | Lt (e1, e2)
  | And (e1, e2)
  | Or (e1, e2) ->
    1 + size_of_expr e1 + size_of_expr e2
  | Not e -> 1 + size_of_expr e
  | Schema ts ->
    1 + List.fold ts ~init:0 ~f:(fun acc t -> acc + size_of_type_ref t)
  | Tuple es -> 1 + List.fold es ~init:0 ~f:(fun acc e -> acc + size_of_expr e)
  | List (es, t) ->
    1
    + List.fold es ~init:0 ~f:(fun acc e -> acc + size_of_expr e)
    + Option.value_map t ~default:0 ~f:size_of_type_ref
  | Proj (e, _) | Tail e -> 2 + size_of_expr e
  | If (e1, e2, e3) -> 1 + size_of_expr e1 + size_of_expr e2 + size_of_expr e3
  | Match (e, branches) ->
    1 + size_of_expr e
    + List.fold branches ~init:0 ~f:(fun acc (p, e) ->
        acc + size_of_expr p + size_of_expr e)
  | Lambda (params, body) -> 1 + List.length params + size_of_expr body
  | Fix (_, params, body) -> 2 + List.length params + size_of_expr body
  | App (_, args) ->
    2 + List.fold args ~init:0 ~f:(fun acc e -> acc + size_of_expr e)

let size_of_stmt = function
  | Decl (_, _ts, App (Filter, [(App (Fold, _) as fold); _])) ->
    size_of_expr fold
  | _ -> raise Unreachable
