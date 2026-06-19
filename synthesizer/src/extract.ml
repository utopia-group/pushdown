open Core
open Ast
open Pass_monos
open Smtlib_utils

(* open Solver *)
open Exns

let rec all_combs = function
  | [] -> []
  | [x] -> [[x]]
  | x :: xs ->
    let rest = all_combs xs in
    rest @ List.map rest ~f:(fun r -> x :: r) @ [[x]]

let all_disjs base =
  base |> all_combs
  |> List.map ~f:(function [conj] -> conj | comb -> mk_or comb)

let wrap_or _U = if List.length _U > 1 then [mk_or _U] else _U

module Case = struct
  module T = struct
    type t = {a : Sexp.t; idx : int; s : Sexp.t} [@@deriving sexp]

    let compare {s = s1; _} {s = s2; _} = Sexp.compare s1 s2

    let show {a; idx; s} =
      sprintf "%s[%d]: %s" (Sexp.to_string_hum a) idx (Sexp.to_string_hum s)

    let mk_r idx s = {a = sexp_unused; idx; s}
  end

  include T
  include Comparable.Make (T)
end

let empty = Set.empty (module Case)
let single = Set.singleton (module Case)

let rec extract_U_Q_expr (m : Utils.state) e a r idx case_i cfg monos vars
    is_bool =
  match e with
  | Bool b -> single {a; idx; s = mk_bool b}
  | Int i -> if is_bool then single {a; idx; s = mk_int i} else empty
  | Float f -> if is_bool then single {a; idx; s = mk_real f} else empty
  | String s -> if is_bool then single {a; idx; s = mk_string s} else empty
  | Var id -> (
    if not is_bool then empty
    else
      let ((id, i) as idx_var), idx_var_sexp = Map.find_exn vars id in
      match Map.find cfg idx_var with
      | Some targets ->
        let _, idx_var_type = get_typed_accr m._State_def 0 i in
        Set.fold targets ~init:empty ~f:(fun acc -> function
          | (id, i), is_ret when Ident.(id = r) && is_ret ->
            let rowi, opt = get_typed_accr m._Row_def 0 i in
            if Sexp.(idx_var_type = sexp_bool) && Sexp.(opt <> sexp_bool) then
              acc
            else
              let ri = rowi *. mk_atom (Ident.show id) in
              Set.add acc
                (Option.value_map
                   (List.find m.decls ~f:(fun (Ident.Ident k, _) ->
                        String.(k = get_atom_str opt)))
                   ~f:(fun (_, (opt_def, _)) ->
                     let value, _ = get_typed_accr opt_def 1 0 in
                     {a; idx = i; s = value *. ri})
                   ~default:Case.{a; idx = i; s = ri})
          | _ -> acc)
      | None ->
        if Ident.(id <> r) then empty
        else single {a; idx = i; s = Option.value_exn idx_var_sexp})
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
    let takes_bool = match e with And _ | Or _ -> true | _ -> false in
    let returns_bool =
      match e with
      | Eq _ | Ge _ | Gt _ | Le _ | Lt _ | And _ | Or _ -> true
      | _ -> false
    in
    let cs1 =
      extract_U_Q_expr m e1 a r idx case_i cfg monos vars
        (returns_bool || is_bool)
    in
    let cs2 =
      extract_U_Q_expr m e2 a r idx case_i cfg monos vars
        (returns_bool || is_bool)
    in
    if Set.is_empty cs1 then if takes_bool then cs2 else empty
    else if Set.is_empty cs2 then if takes_bool then cs1 else empty
    else
      List.fold
        (List.cartesian_product (Set.to_list cs1) (Set.to_list cs2))
        ~init:empty
        ~f:(fun acc (c1, c2) ->
          let op =
            match e with
            | Add _ -> mk_add
            | Sub _ -> mk_sub
            | Mul _ -> mk_mul
            | Div _ -> mk_div
            | Eq _ -> ( =. )
            | Ge _ -> ( >=. )
            | Gt _ -> ( >. )
            | Le _ -> ( <=. )
            | Lt _ -> ( <. )
            | And _ -> ( &&. )
            | Or _ -> ( ||. )
            | _ -> raise Unreachable
          in
          match e with
          | (Eq _ | Ge _ | Gt _ | Le _ | Lt _) when Sexp.(c1.s = c2.s) -> acc
          | Eq _ when Sexp.(a = mk_atom "r") ->
            let case_i =
              Option.value_or_thunk case_i ~default:(fun _ ->
                  match e1 with Proj (Var _, i) -> i | _ -> 0
                  (* raise Unreachable *))
            in
            let op' =
              if
                Set.exists
                  (Map.find_exn cfg (Ident "a", case_i))
                  ~f:(fun ((id, idx), is_ret) ->
                    Ident.(id = r) && idx = c1.idx && is_ret)
              then
                match List.nth_exn monos case_i with
                | Irr _ -> op
                | Inc _ -> ( >=. )
                | Dec _ -> ( <=. )
              else op
            in
            Set.add acc {a; idx = c1.idx; s = op' c1.s c2.s}
          | And _ when Sexp.(a = mk_atom (Ident.show r)) ->
            Set.add (Set.add acc c1) c2
          (* assume LHS is always var *)
          | _ -> Set.add acc {a; idx = c1.idx; s = op c1.s c2.s})
  | Not e ->
    Set.map
      (module Case)
      (extract_U_Q_expr m e a r idx case_i cfg monos vars true)
      ~f:(fun c -> {c with s = ~.(c.s)})
  | Tuple es | List (es, _) ->
    List.foldi es ~init:empty ~f:(fun i acc e ->
        Set.union acc (extract_U_Q_expr m e a r i case_i cfg monos vars is_bool))
  | Proj (Var id, i) -> (
    if not is_bool then empty
    else
      let (id, _), _ = Map.find_exn vars id in
      match Map.find cfg (id, i) with
      | Some targets ->
        let _, idx_var_type = get_typed_accr m._State_def 0 i in
        Set.fold targets ~init:empty ~f:(fun acc -> function
          | (id, i), is_ret when Ident.(id = r) && is_ret ->
            let rowi, opt = get_typed_accr m._Row_def 0 i in
            if Sexp.(idx_var_type = sexp_bool) && Sexp.(opt <> sexp_bool) then
              empty
            else
              let ri = rowi *. mk_atom (Ident.show id) in
              Set.add acc
                (Option.value_map
                   (List.find m.decls ~f:(fun (Ident.Ident k, _) ->
                        String.(k = get_atom_str opt)))
                   ~f:(fun (_, (opt_def, _)) ->
                     let value, _ = get_typed_accr opt_def 1 0 in
                     Case.{a; idx = i; s = value *. ri})
                   ~default:Case.{a; idx = i; s = ri})
          | _ -> acc)
      | None ->
        if Ident.(id <> r) then empty
        else
          let rowi, _ = get_typed_accr m._Row_def 0 i in
          single {a; idx = i; s = rowi *. mk_atom (Ident.show id)})
  | If (e1, e2, e3) ->
    let c1s = extract_U_Q_expr m e1 a r idx case_i cfg monos vars true in
    let c2s = extract_U_Q_expr m e2 a r idx case_i cfg monos vars is_bool in
    let c3s = extract_U_Q_expr m e3 a r idx case_i cfg monos vars is_bool in
    if Set.is_empty c1s then Set.union c2s c3s
    else
      let c12s =
        if Set.is_empty c2s then c1s
        else
          Set.of_list
            (module Case)
            (List.map
               (List.cartesian_product (Set.to_list c1s) (Set.to_list c2s))
               ~f:(fun (c1, c2) -> {c1 with s = c1.s &&. c2.s}))
      in
      let c13s =
        if Set.is_empty c3s then c1s
        else
          Set.of_list
            (module Case)
            (List.map
               (List.cartesian_product (Set.to_list c1s) (Set.to_list c3s))
               ~f:(fun (c1, c3) -> {c1 with s = ~.(c1.s) &&. c3.s}))
      in
      Set.union c12s c13s
  | App (Other (Ident f), es) -> (
    match f with
    | "len" -> (
      match es with
      | [e] ->
        Set.map
          (module Case)
          (extract_U_Q_expr m e a r idx case_i cfg monos vars is_bool)
          ~f:(fun c -> {c with s = sexp_strlen *. c.s})
      | _ -> raise Unreachable)
    | _ -> raise Unreachable)
  | Match (Proj (Var id, i), branches) -> (
    let (id, _), _ = Map.find_exn vars id in
    match
      List.map branches ~f:(fun (pat, e) ->
          match pat with
          | Null _ -> extract_U_Q_expr m e a r idx case_i cfg monos vars is_bool
          | Var id' when Ident.(id = r) ->
            let rowi, opt = get_typed_accr m._Row_def 0 i in
            let ri = rowi *. mk_atom (Ident.show id) in
            let opt_def =
              List.find_map_exn m.decls ~f:(fun (Ident.Ident k, (opt_def, _)) ->
                  Option.some_if String.(k = get_atom_str opt) opt_def)
            in
            let is_some = get_recg opt_def 1 in
            let value, _ = get_typed_accr opt_def 1 0 in
            Set.map
              (module Case)
              (extract_U_Q_expr m e a r idx case_i cfg monos
                 (Map.set vars ~key:id' ~data:((id, i), Some (value *. ri)))
                 is_bool)
              ~f:(fun c -> {c with s = is_some *. ri &&. c.s})
          | Var id' ->
            extract_U_Q_expr m e a r idx (Some i) cfg monos
              (Map.set vars ~key:id' ~data:((id, i), None))
              is_bool
          | _ -> raise Unreachable)
    with
    | [none; some] ->
      if Set.is_empty none then some
      else if Set.is_empty some then none
      else
        List.cartesian_product (Set.to_list none) (Set.to_list some)
        |> List.map ~f:(fun (none_c, some_c) ->
            Case.
              {
                a;
                idx = some_c.idx;
                s =
                  (if Sexp.(a = mk_atom "r") && Sexp.(none_c.s = sexp_true) then
                     some_c.s
                   else none_c.s ||. some_c.s);
              })
        |> Set.of_list (module Case)
    | _ -> raise Unreachable)
  | _ ->
    printf "%s\n" (show_expr e);
    raise Unimplemented

(** Construct input-side predicate universe *)
let extract_U_Q (m : Utils.state) ast (cfg, rev_cfg) monos =
  match ast with
  | Decl
      ( _,
        _,
        App
          ( Filter,
            [App (Fold, [_; _; Lambda ([a'; r'], body)]); Lambda ([a''], _P)] )
      ) ->
    let a = Ident.Ident "a" in
    let r = Ident.Ident "r" in
    let a_sexp = mk_atom "a" in
    let r_sexp = mk_atom "r" in
    let row_recgs =
      List.map (get_typed_accrs m._Row_def 0) ~f:(fun (rowi, field_type) ->
          Option.map
            (List.find m.decls ~f:(fun (Ident.Ident k, _) ->
                 String.(k = get_atom_str field_type)))
            ~f:(fun (_, (opt_def, _)) ->
              let is_somei = get_recg opt_def 1 in
              is_somei *. (rowi *. r_sexp)))
    in
    (* Extract P components *)
    let cases_P =
      Set.map
        (module Case)
        (extract_U_Q_expr m _P r_sexp r' 0 None cfg monos
           (Map.of_alist_exn
              (module Ident)
              [(a'', ((a, 0), None)); (r', ((r, 0), None))])
           false)
        ~f:(fun c ->
          Option.value_map
            (List.nth_exn row_recgs c.idx)
            ~f:(fun row_recg -> {c with s = row_recg &&. c.s})
            ~default:c)
    in
    (* Extract UDF components *)
    let cases_f =
      extract_U_Q_expr m body a_sexp r' 0 None cfg monos
        (Map.of_alist_exn
           (module Ident)
           [(a', ((a, 0), None)); (r', ((r, 0), None))])
        false
    in
    let cases = Set.to_list (Set.union cases_P cases_f) in
    (* printf "Cases:\n%s\n"
        (cases
        |> List.map ~f:(fun c -> "- " ^ Case.show c)
        |> String.concat ~sep:"\n"); *)
    let _U_Q_P, _U_Q_f =
      List.partition_map cases ~f:(fun c ->
          match a with _ when Sexp.(c.a = r_sexp) -> First c | _ -> Second c)
    in
    let _U_Q_P_major =
      List.filter _U_Q_P ~f:(fun c ->
          match Map.find rev_cfg (r, c.idx) with
          | Some sinks ->
            Set.exists sinks ~f:(fun (_, i) ->
                match List.nth_exn monos i with
                | Inc _ | Dec _ -> true
                | _ -> false)
          | None -> false)
    in
    let _U_Q_P = List.map _U_Q_P ~f:(fun c -> c.s) in
    let _U_Q_P_major = List.map _U_Q_P_major ~f:(fun c -> c.s) in
    let _U_Q_f = List.map _U_Q_f ~f:(fun c -> c.s) in
    let _U_Q_Pf = _U_Q_P @ _U_Q_f in
    (* printf "U_Q_Pf:\n%s\n%!"
      (_U_Q_P
      |> List.map ~f:(fun s -> sprintf "- %s" (Sexp.to_string_hum s))
      |> String.concat ~sep:"\n");
    printf "all U_Q_Pf:\n%s\n%!"
      (all_disjs _U_Q_P
      |> List.map ~f:(fun s -> sprintf "- %s" (Sexp.to_string_hum s))
      |> String.concat ~sep:"\n"); *)
    let _U_Q =
      Set.to_list
        (Set.of_list
           (module Sexp)
           (List.filter_map row_recgs ~f:Fn.id
           @ _U_Q_P @ wrap_or _U_Q_P_major @ wrap_or _U_Q_P
           (* @ all_disjs _U_Q_P *)
           @ wrap_or _U_Q_f
           @
           if (not (List.is_empty _U_Q_P)) && not (List.is_empty _U_Q_f) then
             [mk_or _U_Q_Pf]
           else []))
    in
    let _U_Q = if List.is_empty _U_Q then [sexp_true] else _U_Q in
    (* printf "|U_Q|: %d\n%!" (List.length _U_Q); *)
    (* printf "U_Q:\n%s\n%!"
      (_U_Q
      |> List.map ~f:(fun s -> sprintf "- %s" (Sexp.to_string_hum s))
      |> String.concat ~sep:"\n"); *)
    _U_Q
  | _ -> raise Unreachable

let rec extract_cases (m : Utils.state) e a idx cfg monos vars : Case.t list =
  match e with
  | Bool b -> [{a; idx; s = mk_bool b}]
  | Int i -> [{a; idx; s = mk_int i}]
  | Float f -> [{a; idx; s = mk_real f}]
  | Var id -> [{a; idx; s = Map.find_exn vars id}]
  | And (e1, e2) | Or (e1, e2) ->
    extract_cases m e1 a idx cfg monos vars
    @ extract_cases m e2 a idx cfg monos vars
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2)
  | Eq (e1, e2)
  | Ge (e1, e2)
  | Gt (e1, e2)
  | Le (e1, e2)
  | Lt (e1, e2) ->
    let op =
      match e with
      | Add _ -> mk_add
      | Sub _ -> mk_sub
      | Mul _ -> mk_mul
      | Div _ -> mk_div
      | Eq _ -> ( =. )
      | Ge _ -> ( >=. )
      | Gt _ -> ( >. )
      | Le _ -> ( <=. )
      | Lt _ -> ( <. )
      | _ -> raise Unreachable
    in
    let Case.{idx = idx1; s = s1; _} =
      List.hd_exn (extract_cases m e1 a idx cfg monos vars)
    in
    let Case.{s = s2; _} =
      List.hd_exn (extract_cases m e2 a idx cfg monos vars)
    in
    let mono = List.nth_exn monos idx1 in
    if
      (match mono with
        | Irr _ -> false
        | (Inc init | Dec init) when Option.is_none init -> false
        | _ -> true)
      && not
           (Set.exists
              (Map.find_exn cfg (Ident.Ident "a", idx1))
              ~f:(fun ((Ident.Ident id, _), is_ret) ->
                String.(id = "r") && is_ret))
    then
      match mono with
      | Inc (Some init) ->
        [
          {a; idx = idx1; s = s1 >=. mk_int init};
          {a; idx = idx1; s = ~.(s1 >=. mk_int init)};
        ]
      | Dec (Some init) ->
        [
          {a; idx = idx1; s = s1 <=. mk_int init};
          {a; idx = idx1; s = ~.(s1 <=. mk_int init)};
        ]
      | _ -> raise Unreachable
    else
      let op' =
        match e with
        | Eq _ -> (
          match mono with Irr _ -> op | Inc _ -> ( >=. ) | Dec _ -> ( <=. ))
        | _ -> op
      in
      (* assume LHS is always var *)
      [{a; idx = idx1; s = op' s1 s2}; {a; idx = idx1; s = ~.(op' s1 s2)}]
  | Not e ->
    List.map (extract_cases m e a idx cfg monos vars) ~f:(fun c ->
        {c with s = ~.(c.s)})
  | Proj (Var _, idx) ->
    let statei, _ = get_typed_accr m._State_def 0 idx in
    [{a; idx; s = statei *. a}]
  | App (Other (Ident f), es) -> (
    match f with
    | "len" -> (
      match es with
      | [e] ->
        List.map (extract_cases m e a idx cfg monos vars) ~f:(fun c ->
            {c with s = sexp_strlen *. c.s})
      | _ -> raise Unreachable)
    | _ -> raise Unreachable)
  | If (e1, e2, e3) ->
    let Case.{idx; s = s1; _} =
      List.hd_exn (extract_cases m e1 a idx cfg monos vars)
    in
    let Case.{s = s2; _} =
      List.hd_exn (extract_cases m e2 a idx cfg monos vars)
    in
    let Case.{s = s3; _} =
      List.hd_exn (extract_cases m e3 a idx cfg monos vars)
    in
    (* assume COND is always var *)
    [{a; idx; s = mk_ite s1 s2 s3}]
  | Match (Proj (Var _, idx), branches) ->
    let statei, opt = get_typed_accr m._State_def 0 idx in
    let ai = statei *. a in
    let opt_def =
      List.find_map_exn m.decls ~f:(fun (Ident.Ident k, (opt_def, _)) ->
          Option.some_if String.(k = get_atom_str opt) opt_def)
    in
    let is_none = get_recg opt_def 0 in
    let is_some = get_recg opt_def 1 in
    let value, _ = get_typed_accr opt_def 1 0 in
    List.concat_map branches ~f:(fun (pat, e) ->
        match pat with
        | Null _ -> [Case.{a; idx; s = is_none *. ai}]
        | Var id ->
          List.map
            (extract_cases m e a idx cfg monos
               (Map.set vars ~key:id ~data:(value *. ai)))
            ~f:(fun case -> {case with s = is_some *. ai &&. case.s})
        | _ -> raise Unreachable)
  | _ ->
    printf "%s\n" (show_expr e);
    raise Unreachable

let gen_validate_conseq udaf s vars =
  udaf
  @ List.map vars ~f:(fun (var, _type) -> mk_declare_const var _type)
  @ [mk_assert s]
  @ [mk_unary (mk_atom "check-sat")]
  |> List.map ~f:(fun u -> Smtlib_utils.sexp_to_smtlib_string u)
  |> String.concat

let gen_validate udaf s l vars =
  udaf
  @ List.map vars ~f:(fun (var, _type) -> mk_declare_const var _type)
  @ [mk_assert (mk_and [s; l])]
  @ [mk_unary (mk_atom "check-sat")]
  |> List.map ~f:(fun u -> Smtlib_utils.sexp_to_smtlib_string u)
  |> String.concat

(** Construct bisimulation invariant universe *)
let extract_U_BI m ast ((cfg, rev_cfg) : Pass_cfa.cfg * Pass_cfa.rev_cfg) monos
    =
  match ast with
  | Decl
      ( _,
        _,
        App
          ( Filter,
            [App (Fold, [_; (Tuple _I | List (_I, _)); _]); Lambda ([a], _P)] )
      ) ->
    let a1 = mk_atom "a1" in
    let a2 = mk_atom "a2" in
    (* Extract P components, instantiate to a1 *)
    let a1_cases =
      extract_cases m _P a1 0 cfg monos (Map.empty (module Ident))
    in
    (* Extract P components, instantiate to a2 *)
    let a2_cases =
      extract_cases m _P a2 0 cfg monos (Map.empty (module Ident))
    in
    (* printf "a1 cases:\n%s\n"
      (a1_cases
      |> List.map ~f:(fun c -> "- " ^ Case.show c)
      |> String.concat ~sep:"\n");
    printf "a2 cases:\n%s\n"
      (a2_cases
      |> List.map ~f:(fun c -> "- " ^ Case.show c)
      |> String.concat ~sep:"\n"); *)
    let _I_fields =
      get_const_fields
        (List.find_map_exn m.decls ~f:(fun (Ident.Ident k, (_I_def, _)) ->
             Option.some_if String.(k = get_atom_str m._I) _I_def))
    in
    (* a1-oriented leaves *)
    let leaves1 =
      List.mapi _I_fields ~f:(fun idx _I_i ->
          let statei, _ = get_typed_accr m._State_def 0 idx in
          let a1i = statei *. a1 in
          let a2i = statei *. a2 in
          [sexp_false; a1i =. a2i])
    in
    (* a2-oriented leaves extracted from initializer *)
    let leaves2 =
      List.mapi _I_fields ~f:(fun idx _I_i ->
          let statei, _ = get_typed_accr m._State_def 0 idx in
          let a1i = statei *. a1 in
          let a2i = statei *. a2 in
          sexp_false :: (a1i =. a2i)
          ::
          (if String.contains (Sexp.to_string _I_i) '_' then []
           else [a2i =. _I_i; ~.(a2i =. _I_i)]))
    in
    let _U_BI_base =
      List.concat_mapi _I ~f:(fun i _ ->
          let a1i_leaves =
            List.concat_map a1_cases ~f:(function
              | {idx; s; _} when idx = i ->
                List.map (List.nth_exn leaves1 i) ~f:(( =>. ) s)
                (* List.filter_map (List.nth_exn leaves1 i) ~f:(fun l ->
                    let encoding = gen_validate_conseq m.udaf l vars12 in
                    if (solve "z3" z3_args encoding).sat then
                      let encoding = gen_validate m.udaf s l vars12 in
                      if (solve "z3" z3_args encoding).sat then Some (s =>. l)
                      else (
                        printf "ayo\n%!";
                        None)
                    else Some (s =>. l)) *)
              | _ -> [])
          in
          let a2i_leaves =
            Set.to_list
              (List.fold a2_cases
                 ~init:(Set.empty (module Sexp))
                 ~f:(fun acc -> function
                   | {idx; s; _} when idx = i ->
                     let targets = Map.find_exn rev_cfg (a, i) in
                     List.foldi leaves2 ~init:acc ~f:(fun idx acc leaves ->
                         if Set.mem targets (a, idx) then
                           leaves
                           |> List.map ~f:(( =>. ) s)
                           (* |> List.filter_map ~f:(fun l ->
                                  let encoding =
                                    gen_validate_conseq m.udaf l vars12
                                  in
                                  if (solve "z3" z3_args encoding).sat then
                                    let encoding =
                                      gen_validate m.udaf s l vars12
                                    in
                                    if (solve "z3" z3_args encoding).sat then
                                      Some (s =>. l)
                                    else (
                                      printf "ayo\n%!";
                                      None)
                                  else Some (s =>. l)) *)
                           |> Set.of_list (module Sexp)
                           |> Set.union acc
                         else acc)
                   | _ -> acc))
          in
          let a2i_a1i_leaves =
            (* List.concat_map a2_cases ~f:(function
              | {idx; s; _} when idx = i -> List.map a1i_leaves ~f:(( =>. ) s)
              | _ -> []) *)
            []
          in
          [a1i_leaves; a2i_leaves; a2i_a1i_leaves])
    in
    let a2i_rest =
      Set.to_list
        (List.foldi _I
           ~init:(Set.empty (module Sexp))
           ~f:(fun i acc _ ->
             Set.fold ~init:acc
               (Map.find_exn cfg (a, i))
               ~f:(fun acc -> function
                 | (var, j), _is_ret
                   when Ident.(var = a) && j <> i (* && _is_ret  *) ->
                   List.fold a2_cases ~init:acc ~f:(fun acc -> function
                     | {idx; s; _} when idx = i ->
                       let a1j_leaves = List.nth_exn _U_BI_base (j * 3) in
                       (* let a2j_leaves = List.nth_exn _U_BI_base ((j * 3) + 1) in *)
                       (* let a2j_a1j_leaves =
                         List.nth_exn _U_BI_base ((j * 3) + 2)
                       in *)
                       (* a2j_leaves *)
                       a1j_leaves
                       |> List.map ~f:(( =>. ) s)
                       |> Set.of_list (module Sexp)
                       |> Set.union acc
                     | _ -> acc)
                 | _ -> acc)))
    in
    let _U_BI =
      List.filter (List.concat leaves1) ~f:(Sexp.( <> ) sexp_false)
      @ Set.to_list (Set.of_list (module Sexp) (List.concat _U_BI_base))
      @ a2i_rest
    in
    (* printf "|U_BI|: %d\n%!" (List.length _U_BI); *)
    (* printf "U_BI:\n%s\n%!"
      (_U_BI |> List.map ~f:Sexp.to_string_hum |> String.concat ~sep:"\n"); *)
    _U_BI
  | _ -> raise Unreachable

type component = {s : Sexp.t; t : Sexp.t}

let rec extract_P_components (m : Utils.state) e vars : component list =
  match e with
  | Bool b -> [{s = mk_bool b; t = sexp_bool}]
  | Int i -> [{s = mk_int i; t = sexp_int}]
  | Float f -> [{s = mk_real f; t = sexp_real}]
  | Var id -> [Map.find_exn vars id]
  | Proj (e, i) ->
    let {s; t} = List.hd_exn (extract_P_components m e vars) in
    let statei, statei_t = get_typed_accr t 0 i in
    [{s = statei *. s; t = statei_t}]
  | Match (e, branches) ->
    let {s; t} = List.hd_exn (extract_P_components m e vars) in
    let opt_def =
      List.find_map_exn m.decls ~f:(fun (Ident.Ident name, (opt_def, _)) ->
          Option.some_if String.(name = get_atom_str t) opt_def)
    in
    List.concat_map branches ~f:(fun (pat, body) ->
        match pat with
        | Null _ ->
          let comps = extract_P_components m body vars in
          List.map comps ~f:(fun c ->
              {c with s = get_recg opt_def 0 *. s =>. c.s})
        | Var id ->
          let value, value_t = get_typed_accr opt_def 1 0 in
          let comps =
            extract_P_components m body
              (Map.set vars ~key:id ~data:{s = value *. s; t = value_t})
          in
          List.map comps ~f:(fun c ->
              {c with s = get_recg opt_def 1 *. s =>. c.s})
        | _ -> raise Unreachable)
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2)
  | Eq (e1, e2)
  | Ge (e1, e2)
  | Gt (e1, e2)
  | Le (e1, e2)
  | Lt (e1, e2) ->
    let comps1 = extract_P_components m e1 vars in
    let comps2 = extract_P_components m e2 vars in
    let op =
      match e with
      | Add _ -> mk_add
      | Sub _ -> mk_sub
      | Mul _ -> mk_mul
      | Div _ -> mk_div
      | Eq _ -> ( =. )
      | Ge _ -> ( >=. )
      | Gt _ -> ( >. )
      | Le _ -> ( <=. )
      | Lt _ -> ( <. )
      | _ -> raise Unreachable
    in
    List.map (List.cartesian_product comps1 comps2) ~f:(fun (c1, c2) ->
        {c1 with s = op c1.s c2.s})
  | Not e ->
    let comps =
      List.map (extract_P_components m e vars) ~f:(fun c ->
          {c with s = ~.(c.s)})
    in
    if List.length comps = 1 then comps
    else
      let comps_hd, comps_tl = (List.hd_exn comps, List.tl_exn comps) in
      [
        List.fold comps_tl ~init:comps_hd ~f:(fun acc c ->
            {c with s = acc.s ||. c.s});
      ]
  | And (e1, e2) ->
    let comps1 = extract_P_components m e1 vars in
    let comps2 = extract_P_components m e2 vars in
    comps1 @ comps2
  | Or (e1, e2) ->
    let comps1 = extract_P_components m e1 vars in
    let comps2 = extract_P_components m e2 vars in
    (*! Don't need to check emptiness *)
    List.map (List.cartesian_product comps1 comps2) ~f:(fun (c1, c2) ->
        {c1 with s = c1.s ||. c2.s})
  | If (e1, e2, e3) ->
    let comps1 = extract_P_components m e1 vars in
    let comps2 = extract_P_components m e2 vars in
    let comps3 = extract_P_components m e3 vars in
    List.map
      (List.cartesian_product comps1 (List.cartesian_product comps2 comps3))
      ~f:(fun (c1, (c2, c3)) -> {c1 with s = mk_ite c1.s c2.s c3.s})
  | _ ->
    printf "%s\n" (show_expr e);
    raise Unreachable

(** Construct residual universe *)
let extract_U_P' (m : Utils.state) ast =
  match ast with
  | Decl (_, _, App (Filter, [App (Fold, _); Lambda ([a'], _P)])) ->
    let a = mk_atom "a" in
    (* Initialize universe with initializer-derived candidates *)
    let init_U_P' =
      m.decls
      |> List.find_map_exn ~f:(fun (Ident.Ident k, (_I_def, _)) ->
          Option.some_if String.(k = get_atom_str m._I) _I_def)
      |> get_const_fields
      |> List.mapi ~f:(fun i _I_i ->
          let statei, statei_t = get_typed_accr m._State_def 0 i in
          let ai = statei *. a in
          if String.contains (Sexp.to_string _I_i) '_' then
            let opt_def =
              List.find_map_exn m.decls
                ~f:(fun (Ident.Ident id, (opt_def, _)) ->
                  Option.some_if String.(id = get_atom_str statei_t) opt_def)
            in
            let is_none = get_recg opt_def 0 in
            (* technically don't need to special case: *)
            is_none *. ai =>. sexp_false
          else ~.(ai =. _I_i))
    in
    (* Extract P components *)
    let _P_comps =
      List.map
        (extract_P_components m _P
           (Map.of_alist_exn (module Ident) [(a', {s = a; t = m._State_def})]))
        ~f:(fun {s; _} -> s)
    in
    (* let before = List.length _P_comps in *)
    (* printf "before: %d\n" before; *)
    let _P_comps = Set.of_list (module Sexp) _P_comps in
    (* let after = Set.length _P_comps in *)
    (* printf "after: %d\n" after; *)
    (* if before <> after then failwith "before <> after"; *)
    (* printf "P_comps:\n%s\n"
        (_P_comps |> Set.to_list
        |> List.map ~f:Sexp.to_string_hum
        |> String.concat ~sep:"\n"); *)
    let _P_def =
      List.find_map_exn m.decls ~f:(fun (Ident id, (_P_def, _)) ->
          Option.some_if String.(id = get_atom_str m._P) _P_def)
    in
    let _P_comps =
      if Set.is_empty _P_comps then
        Set.singleton (module Sexp) (get_fun_body _P_def)
      else _P_comps
    in
    let _U_P' =
      Set.to_list (Set.union _P_comps (Set.of_list (module Sexp) init_U_P'))
    in
    (* printf "U_P':\n%s\n%!"
      (_U_P'
      |> List.map ~f:(fun u -> "- " ^ Sexp.to_string_hum u)
      |> String.concat ~sep:"\n"); *)
    (_U_P', _P_comps)
  | _ -> raise Unreachable

(* Construct three universes: input-side predicate, bisimulation invariant, residual predicate *)
let extract_Us m ast (cfgs : Pass_cfa.cfg * Pass_cfa.rev_cfg) monos =
  let _U_Q = extract_U_Q m ast cfgs monos in
  let _U_BI = extract_U_BI m ast cfgs monos in
  let _U_P', _P_comps = extract_U_P' m ast in
  (_U_Q, _U_BI, _U_P', _P_comps)
