open Core
open Ast
open Exns

type dataflows = Set.M(Ident).t Map.M(Ident).t

let rec compute_expr_outflows outflows user expr =
  match expr with
  | Bool _ | Int _ | Float _ | String _ | Null _ -> outflows
  | Var id -> (
    match Map.find outflows id with
    | None ->
      Map.add_exn outflows ~key:id ~data:(Set.singleton (module Ident) user)
    | Some users ->
      Map.add_exn (Map.remove outflows id) ~key:id ~data:(Set.add users user))
  | Schema _ -> outflows
  | Tuple es | List (es, _) ->
    List.fold es ~init:outflows ~f:(fun outflows e ->
        compute_expr_outflows outflows user e)
  | Proj (e, _) | Tail e -> compute_expr_outflows outflows user e
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
    let e1_outflows = compute_expr_outflows outflows user e1 in
    compute_expr_outflows e1_outflows user e2
  | Not e -> compute_expr_outflows outflows user e
  | If (e1, e2, e3) ->
    let e1_outflows = compute_expr_outflows outflows user e1 in
    let e2_outflows = compute_expr_outflows e1_outflows user e2 in
    compute_expr_outflows e2_outflows user e3
  | Lambda (params, e) | Fix (_, params, e) ->
    if List.exists params ~f:(Ident.( = ) user) then outflows
    else compute_expr_outflows outflows user e
  | App (_, es) ->
    List.fold es ~init:outflows ~f:(fun outflows e ->
        compute_expr_outflows outflows user e)
  | Match (e, cases) ->
    let e_outflows = compute_expr_outflows outflows user e in
    List.fold_right cases ~init:e_outflows ~f:(fun (_, e) outflows ->
        compute_expr_outflows outflows user e)

let compute_outflows prog =
  List.fold prog
    ~init:(Map.empty (module Ident))
    ~f:(fun outflows -> function
      | Decl (ident, _, expr) -> compute_expr_outflows outflows ident expr)

(* Top-level entry: remove identifiers that no live use depends on. *)
let gc prog =
  let outflows = compute_outflows prog in
  (* Format.printf "Outflows:\n%s\n"
    (outflows |> Map.to_alist
    |> List.map ~f:(fun (k, v) ->
           Format.asprintf "%a: %s" Ident.pp k
             (v |> Set.to_list
             |> List.map ~f:(fun id -> Format.asprintf "%a" Ident.pp id)
             |> String.concat ~sep:","))
    |> String.concat ~sep:"\n"); *)
  match
    List.filter prog ~f:(function Decl (ident, _, _) ->
        Option.is_some (Map.find outflows ident))
  with
  | [] -> [List.last_exn prog]
  | prog' -> prog'

let rec compute_expr_inflows inflows target expr =
  match expr with
  | Bool _ | Int _ | Float _ | Null _ -> inflows
  | Var id ->
    Map.add_exn
      (Map.remove inflows target)
      ~key:target
      ~data:(Set.add (Map.find_exn inflows target) id)
  | Schema _ -> inflows
  | Tuple es ->
    List.fold es ~init:inflows ~f:(fun inflows e ->
        compute_expr_inflows inflows target e)
  | Proj (e, _) -> compute_expr_inflows inflows target e
  | Add (e1, e2) | Gt (e1, e2) ->
    let e1_inflows = compute_expr_inflows inflows target e1 in
    compute_expr_inflows e1_inflows target e2
  | App (_, es) ->
    List.fold es ~init:inflows ~f:(fun inflows e ->
        compute_expr_inflows inflows target e)
  | _ ->
    Format.printf "%a\n" pp_expr expr;
    raise Unimplemented

let compute_inflows prog =
  List.fold prog
    ~init:(Map.empty (module Ident))
    ~f:(fun inflows -> function
      | Decl (ident, _, expr) ->
        compute_expr_inflows
          (Map.add_exn inflows ~key:ident ~data:(Set.empty (module Ident)))
          ident expr)

let compute_order inflows : Ident.t list =
  let rec compute_order' inflows =
    match List.filter inflows ~f:(fun (_, inflow) -> Set.is_empty inflow) with
    | [] -> []
    | (target, inflow) :: rest ->
      if Set.is_empty inflow then
        target
        :: compute_order'
             (List.map
                (List.filter inflows ~f:(fun (target', _) ->
                     Ident.(target' <> target)))
                ~f:(fun (target', inflow') ->
                  (target', Set.remove inflow' target)))
      else compute_order' rest
  in
  compute_order' (Map.to_alist inflows)
