open Core
open Ast
open Exns

module Indexed_ident = struct
  module T = struct
    type t = Ident.t * int [@@deriving compare, sexp]

    let pp fmt (Ident.Ident x, idx) = Format.fprintf fmt "%s[%d]" x idx
    let show = Format.asprintf "%a" pp
  end

  include T
  include Comparable.Make (T)
end

module Labeled_ident = struct
  module T = struct
    type t = Indexed_ident.t * bool [@@deriving compare, sexp]

    let pp fmt (idx_id, is_ret) =
      if is_ret then Format.fprintf fmt "%a" Indexed_ident.pp idx_id
      else Format.fprintf fmt "(%a)" Indexed_ident.pp idx_id

    let show = Format.asprintf "%a" pp
  end

  include T
  include Comparable.Make (T)
end

type cfg = Set.M(Labeled_ident).t Map.M(Indexed_ident).t
type rev_cfg = Set.M(Indexed_ident).t Map.M(Indexed_ident).t

(* let has_conditional = ref false *)

let rec analyze_expr e sink vars is_ret ((cfg, rev_cfg) : cfg * rev_cfg) =
  match e with
  | Bool _ | Int _ | Float _ | Null _ | String _ -> (cfg, rev_cfg)
  | Var id ->
    let src = Map.find_exn vars id in
    ( Map.set cfg ~key:sink
        ~data:
          (Set.add
             (Option.value (Map.find cfg sink)
                ~default:(Set.empty (module Labeled_ident)))
             (src, is_ret)),
      Map.set rev_cfg ~key:src
        ~data:
          (Set.add
             (Option.value (Map.find rev_cfg src)
                ~default:(Set.empty (module Indexed_ident)))
             sink) )
  | Proj (Var id, i) ->
    let src = (id, i) in
    ( Map.set cfg ~key:sink
        ~data:
          (Set.add
             (Option.value (Map.find cfg sink)
                ~default:(Set.empty (module Labeled_ident)))
             (src, is_ret)),
      Map.set rev_cfg ~key:src
        ~data:
          (Set.add
             (Option.value (Map.find rev_cfg src)
                ~default:(Set.empty (module Indexed_ident)))
             sink) )
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
    (cfg, rev_cfg)
    |> analyze_expr e1 sink vars is_ret
    |> analyze_expr e2 sink vars is_ret
  | Not e -> analyze_expr e sink vars is_ret (cfg, rev_cfg)
  | Tuple es | List (es, _) ->
    List.foldi es ~init:(cfg, rev_cfg) ~f:(fun i acc e ->
        analyze_expr e (fst sink, i) vars is_ret acc)
  | App (_, es) ->
    List.fold es ~init:(cfg, rev_cfg) ~f:(fun acc e ->
        analyze_expr e sink vars is_ret acc)
  | Tail e -> analyze_expr e sink vars is_ret (cfg, rev_cfg)
  | If (e1, e2, e3) ->
    (* has_conditional := true; *)
    (cfg, rev_cfg)
    |> analyze_expr e1 sink vars false
    |> analyze_expr e2 sink vars is_ret
    |> analyze_expr e3 sink vars is_ret
  | Match ((Proj (Var id, i) as e), cases) ->
    List.fold cases
      ~init:(analyze_expr e sink vars false (cfg, rev_cfg))
      ~f:(fun acc (pat, e) ->
        match pat with
        | Null _ -> analyze_expr e sink vars is_ret acc
        | Var id' ->
          analyze_expr e sink (Map.set vars ~key:id' ~data:(id, i)) is_ret acc
        | _ -> raise Unreachable)
  | _ -> raise Unreachable

let print_cfg label cfg show =
  cfg |> Map.to_alist
  |> List.map ~f:(fun (k, v) ->
      v |> Set.to_list |> List.map ~f:show |> String.concat ~sep:", "
      |> Format.sprintf "- %s -> %s" (Indexed_ident.show k))
  |> String.concat ~sep:"\n"
  |> Format.printf "%s:\n%s\n%!" label

(* Build the forward and reverse control-flow graphs for the UDF. *)
let analyze ast =
  match ast with
  | Decl (_, _, App (Filter, [App (Fold, [_; _; Lambda ([a; _], body)]); _])) ->
    let cfgs =
      analyze_expr body (a, 0)
        (Map.empty (module Ident))
        true
        (Map.empty (module Indexed_ident), Map.empty (module Indexed_ident))
    in
    (* printf "%b," !has_conditional; *)
    cfgs
  | _ -> raise Unreachable
