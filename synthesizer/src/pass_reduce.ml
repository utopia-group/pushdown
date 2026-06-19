open Core
open Ast
open Exns

module With_state = struct
  module T = struct
    type state = expr Map.M(Ident).t
    type 'a t = state -> 'a * state

    let return (a : 'a) : 'a t = fun st -> (a, st)

    let bind (m : 'a t) ~(f : 'a -> 'b t) : 'b t =
     fun st ->
      let a, st' = m st in
      f a st'

    let map = `Define_using_bind
    let get_state () : state t = fun st -> (st, st)

    let add_decl (id : Ident.t) (e : expr) : unit t =
     fun st -> ((), Map.set (Map.remove st id) ~key:id ~data:e)

    let set_state (st : state) : unit t = fun _ -> ((), st)
  end

  include T
  include Monad.Make (T)
end

open With_state
open With_state.Let_syntax

let rec reduce_expr e decl_type : expr With_state.t =
  match e with
  | Bool _ | Int _ | Float _ | String _ -> return e
  | Var id -> (
    let%bind decls = get_state () in
    match Map.find decls id with Some e' -> return e' | None -> return e)
  | Null _ -> (
    match decl_type with
    | Some [t] -> return (Null (Some t))
    | _ -> raise (Compile_error (1, "Cannot infer type of None")))
  | Schema _ | Lambda _ | Fix _ -> return e
  | Tuple es ->
    let%bind st = get_state () in
    (match decl_type with
      | Some field_types ->
        es
        |> List.foldi ~init:[] ~f:(fun i acc e ->
            fst
              (reduce_expr e
                 (Option.map (List.nth field_types i) ~f:(fun field_type ->
                      [field_type]))
                 st)
            :: acc)
        |> List.rev
      | None ->
        List.fold_right es ~init:[] ~f:(fun e acc ->
            fst (reduce_expr e decl_type st) :: acc))
    |> Tuple |> return
  | List (es, _) ->
    let%bind st = get_state () in
    let es' =
      match decl_type with
      | Some field_types when List.length field_types = List.length es ->
        es
        |> List.mapi ~f:(fun i e ->
            fst
              (reduce_expr e
                 (Option.map (List.nth field_types i) ~f:(fun field_type ->
                      [field_type]))
                 st))
      | _ -> List.map es ~f:(fun e -> fst (reduce_expr e decl_type st))
    in
    let elem_type =
      match decl_type with Some (t :: _) -> Some t | _ -> None
    in
    return (List (es', elem_type))
  | Proj (e, i) ->
    let%bind e' = reduce_expr e decl_type in
    return (Proj (e', i))
  | Tail e ->
    let%bind e' = reduce_expr e decl_type in
    return (Tail e')
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
    let%bind e1' = reduce_expr e1 decl_type in
    let%bind e2' = reduce_expr e2 decl_type in
    return
      (match e with
      | Add _ -> Add (e1', e2')
      | Sub _ -> Sub (e1', e2')
      | Mul _ -> Mul (e1', e2')
      | Div _ -> Div (e1', e2')
      | Eq _ -> Eq (e1', e2')
      | Ge _ -> Ge (e1', e2')
      | Gt _ -> Gt (e1', e2')
      | Le _ -> Le (e1', e2')
      | Lt _ -> Lt (e1', e2')
      | And _ -> And (e1', e2')
      | Or _ -> Or (e1', e2')
      | _ -> raise Unreachable)
  | Not e ->
    let%bind e' = reduce_expr e decl_type in
    return (Not e')
  | If (e1, e2, e3) ->
    let%bind e1' = reduce_expr e1 decl_type in
    let%bind e2' = reduce_expr e2 decl_type in
    let%bind e3' = reduce_expr e3 decl_type in
    return (If (e1', e2', e3'))
  | App (f, es) ->
    let%bind st = get_state () in
    return (App (f, List.map es ~f:(fun e -> fst (reduce_expr e decl_type st))))
  | Match (e, cases) ->
    let%bind decls = get_state () in
    let%bind e' = reduce_expr e decl_type in
    let cases' =
      List.fold_right cases ~init:[] ~f:(fun (pat, body) acc ->
          let body', _ =
            reduce_expr body decl_type
              (match pat with Var id -> Map.remove decls id | _ -> decls)
          in
          (pat, body') :: acc)
    in
    return (Match (e', cases'))

(* Top-level entry: normalize a parsed program. *)
let reduce prog =
  prog
  |> List.fold
       ~init:([], Map.empty (module Ident))
       ~f:(fun (stmts, st) (Decl (id, decl_type, e)) ->
         let e', _ = reduce_expr e decl_type st in
         let stmt' = Decl (id, decl_type, e') in
         (stmts @ [stmt'], snd (add_decl id e' st)))
  |> fst
