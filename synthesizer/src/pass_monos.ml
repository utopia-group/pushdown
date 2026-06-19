open Core
open Ast
open Exns

type mono = Inc of int option | Dec of int option | Irr of int option
[@@deriving show {with_path = false}]

let rec infer_monos_expr expr a r idx cfg vars _I =
  match expr with
  | Bool _ | Int _ | Float _ | String _ | Var _ | Proj _ | Eq _ -> [Irr None]
  | Add (e1, e2) -> (
    match (e1, e2) with
    | Proj (Var id, _), Int i when i >= 0 ->
      if Ident.(id = a) then
        [
          Inc
            (Some
               (match List.nth_exn _I idx with
               | Int i -> i
               | _ -> raise Unreachable));
        ]
      else raise Unreachable
    | _ -> [Irr None])
  | Ge (e1, e2) | Gt (e1, e2) | Le (e1, e2) | Lt (e1, e2) ->
    let init =
      match List.nth_exn _I idx with
      | Int i -> Some i
      | Float f -> Some (Float.to_int f)
      | _ -> None
    in
    let e1 = match e1 with Var id -> Map.find_exn vars id | _ -> e1 in
    let e2 = match e2 with Var id -> Map.find_exn vars id | _ -> e2 in
    [
      (match e1 with
      | Proj (Var id, i) when Ident.(id = r) ->
        if
          Set.exists
            (Map.find_exn cfg (a, idx))
            ~f:(fun ((id', i'), is_ret) -> Ident.(id' = id) && i' = i && is_ret)
        then
          match expr with
          | Ge _ | Gt _ -> Inc init
          | Le _ | Lt _ -> Dec init
          | _ -> raise Unreachable
        else Irr None
      | Proj (Var id, _) when Ident.(id = a) -> (
        match e2 with
        | Proj (Var id, i) when Ident.(id = r) ->
          if
            Set.exists
              (Map.find_exn cfg (a, idx))
              ~f:(fun ((id', i'), is_ret) ->
                Ident.(id' = id) && i' = i && is_ret)
          then
            match expr with
            | Ge _ | Gt _ -> Dec init
            | Le _ | Lt _ -> Inc init
            | _ -> raise Unreachable
          else Irr None
        | _ -> raise Unreachable)
      | _ -> raise Unreachable);
    ]
  | Not e ->
    let mono = List.hd_exn (infer_monos_expr e a r idx cfg vars _I) in
    [
      (match mono with
      | Irr _ -> mono
      | Inc init -> Dec init
      | Dec init -> Inc init);
    ]
  | And (e1, e2) ->
    let mono1 = List.hd_exn (infer_monos_expr e1 a r idx cfg vars _I) in
    let mono2 = List.hd_exn (infer_monos_expr e2 a r idx cfg vars _I) in
    [
      (match (mono1, mono2) with
      | Irr _, Irr _ | Inc _, Dec _ | Dec _, Inc _ -> Irr None
      | Irr _, Inc init | Inc init, Irr _ -> Inc init
      | Irr _, Dec init | Dec init, Irr _ -> Dec init
      | Inc init1, Inc init2 ->
        Inc
          Option.(
            init1 >>= fun i1 ->
            init2 >>= fun i2 -> Some (min i1 i2))
      | Dec init1, Dec init2 ->
        Dec
          Option.(
            init1 >>= fun i1 ->
            init2 >>= fun i2 -> Some (max i1 i2)));
    ]
  | Or (e1, e2) ->
    let mono1 = List.hd_exn (infer_monos_expr e1 a r idx cfg vars _I) in
    let mono2 = List.hd_exn (infer_monos_expr e2 a r idx cfg vars _I) in
    [
      (match (mono1, mono2) with
      | Irr _, Irr _ | Inc _, Dec _ | Dec _, Inc _ -> Irr None
      | Irr _, Inc _ | Inc _, Irr _ -> Irr None
      | Irr _, Dec _ | Dec _, Irr _ -> Irr None
      | Inc init1, Inc init2 ->
        Inc
          Option.(
            init1 >>= fun i1 ->
            init2 >>= fun i2 -> Some (min i1 i2))
      | Dec init1, Dec init2 ->
        Dec
          Option.(
            init1 >>= fun i1 ->
            init2 >>= fun i2 -> Some (max i1 i2)));
    ]
  | If (e1, e2, _) ->
    let mono1 = List.hd_exn (infer_monos_expr e1 a r idx cfg vars _I) in
    let mono2 = List.hd_exn (infer_monos_expr e2 a r idx cfg vars _I) in
    let e2 = match e2 with Var id -> Map.find_exn vars id | _ -> e2 in
    [
      (match e2 with
      | Proj (Var id, _) | Var id ->
        if Ident.(id = r) then mono1
        else if Ident.(id = a) then
          match mono1 with
          | Irr _ -> mono1
          | Inc init -> Dec init
          | Dec init -> Inc init
        else raise Unreachable
      | Add _ -> (
        match mono1 with
        | Inc (Some init) when init >= 0 -> mono1
        | _ -> Irr None)
      | _ -> mono2);
    ]
  | Match ((Proj _ as proj), branches) ->
    [
      List.fold branches ~init:(Irr None) ~f:(fun mono (pat, e) ->
          let mono' =
            List.hd_exn
              (infer_monos_expr e a r idx cfg
                 (match pat with
                 | Null _ -> vars
                 | Var id' -> Map.set vars ~key:id' ~data:proj
                 | _ -> raise Unreachable)
                 _I)
          in
          match mono' with
          | Inc init' -> (
            match mono with
            | Dec _ -> Irr None
            | Irr init | Inc init ->
              Inc
                Option.(
                  init' >>| fun i' -> min i' (Option.value init ~default:i')))
          | Dec init' -> (
            match mono with
            | Inc _ -> Irr None
            | Irr init | Dec init ->
              Dec
                Option.(
                  init' >>| fun i' -> max i' (Option.value init ~default:i')))
          | Irr _ as irr -> irr);
    ]
  | Tuple es | List (es, _) ->
    List.concat_mapi es ~f:(fun idx e -> infer_monos_expr e a r idx cfg vars _I)
  | _ ->
    printf "%s\n" (show_expr expr);
    raise Unreachable

(* Top-level entry: infer the monotonicity of every accumulator slot. *)
let infer_monos ast cfg =
  match ast with
  | Decl
      ( _,
        _,
        App
          ( Filter,
            [
              App (Fold, [_; (Tuple _I | List (_I, _)); Lambda ([a; r], body)]);
              _;
            ] ) ) ->
    infer_monos_expr body a r 0 cfg
      (Map.of_alist_exn (module Ident) [(a, Var a); (r, Var r)])
      _I
  | _ -> raise Unreachable
