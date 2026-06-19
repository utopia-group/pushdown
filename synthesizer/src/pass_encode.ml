open Core
open Ast
open Smtlib_utils
open Exns

module With_state = struct
  module T = struct
    type typed_decl = Sexp.t * Sexp.t
    type type_sig = Sexp.t list * Sexp.t

    type state = {
      decls : typed_decl Map.M(Ident).t;
      seen : Set.M(Sexp).t;
      type_sigs : type_sig Map.M(Ident).t;
      order : Ident.t list;
      is_type : bool;
      ret_type : Sexp.t option;
    }

    type 'a t = state -> 'a * state

    let return (a : 'a) : 'a t = fun st -> (a, st)

    let bind (m : 'a t) ~(f : 'a -> 'b t) : 'b t =
     fun st ->
      let a, st' = m st in
      f a st'

    let map = `Define_using_bind
    let get_state () : state t = fun st -> (st, st)

    let add_decl (id : Ident.t) (decl : typed_decl) : unit t =
     fun ({decls; seen; order; _} as st) ->
      ( (),
        {
          st with
          decls =
            (if Set.mem seen (fst decl) then decls
             else Map.set decls ~key:id ~data:decl);
          seen = Set.add seen (fst decl);
          order =
            (if Set.mem seen (fst decl) || Map.mem decls id then order
             else id :: order);
        } )

    let add_type_sig (id : Ident.t) (type_sig : type_sig) : unit t =
     fun ({type_sigs; _} as st) ->
      ((), {st with type_sigs = Map.set type_sigs ~key:id ~data:type_sig})

    let set_state (st : state) : unit t = fun _ -> ((), st)
    let set_is_type (is_type : bool) : unit t = fun st -> ((), {st with is_type})

    let set_ret_type (ret_type : Sexp.t option) : unit t =
     fun st -> ((), {st with ret_type})
  end

  include T
  include Monad.Make (T)
end

let init : With_state.state =
  {
    decls = Map.empty (module Ident);
    seen = Set.empty (module Sexp);
    type_sigs = Map.empty (module Ident);
    order = [];
    is_type = true;
    ret_type = None;
  }

open With_state
open With_state.Let_syntax

let ast_size = ref 0
let incr_ast_size is_rel x = if is_rel then ast_size := !ast_size + x

let rec encode_expr name e is_rel =
  (* incr_ast_size is_rel 1; *)
  match e with
  | Bool b -> return (mk_bool b, sexp_bool)
  | Int i -> return (mk_int i, sexp_int)
  | Float i -> return (mk_real i, sexp_real)
  | String s -> return (mk_string s, sexp_string)
  | Var id ->
    let%bind {decls; _} = get_state () in
    return (Map.find_exn decls id)
  | Null _ ->
    (* incr_ast_size is_rel (Option.value_map t ~default:0 ~f:type_ref_size); *)
    return (sexp_null, sexp_unused)
  | Schema type_refs ->
    (* incr_ast_size is_rel
      (List.fold type_refs ~init:0 ~f:(fun acc t -> acc + type_ref_size t)); *)
    let datatype_name = Format.sprintf "%s_Row" name in
    let%bind () =
      List.fold type_refs ~init:(return ()) ~f:(fun acc -> function
        | OptionType inner_type ->
          let%bind () = acc in
          let opt_name = type_ref_to_opt inner_type in
          let opt =
            mk_option opt_name
              (mk_atom (Format.asprintf "%a" pp_type_ref inner_type))
          in
          add_decl (Ident opt_name) (opt, mk_atom opt_name)
        | _ -> acc)
    in
    let datatype =
      mk_datatype datatype_name
        (fun mk_constr -> [mk_constr 0])
        (fun mk_accr ->
          List.mapi type_refs ~f:(fun i -> function
            | BoolType -> mk_accr 0 i sexp_bool
            | IntType -> mk_accr 0 i sexp_int
            | FloatType -> mk_accr 0 i sexp_real
            | StringType -> mk_accr 0 i sexp_string
            | OptionType type_ref ->
              let opt_name = type_ref_to_opt type_ref in
              mk_accr 0 i (mk_atom opt_name)
            | ListType _ -> raise Unimplemented))
    in
    (* let _Q_name = Format.sprintf "%s_Q" name in *)
    let sexp_datatype_name = mk_atom datatype_name in
    (* let _Q = mk_declare_fun _Q_name [sexp_datatype_name] sexp_bool in *)
    let%bind () =
      add_decl (Ident datatype_name) (datatype, sexp_datatype_name)
    in
    (* and () = add_decl (Ident _Q_name) (_Q, sexp_bool) in *)
    return (datatype, sexp_datatype_name)
  | Tuple es | List (es, _) -> (
    let%bind st = get_state () in
    if st.is_type then
      let%bind () =
        List.fold es ~init:(return ()) ~f:(fun acc -> function
          | Null null_type ->
            let%bind () = acc in
            let inner_type =
              match Option.value_exn null_type with
              | OptionType t -> t
              | _ -> raise Unreachable
            in
            let opt_name = type_ref_to_opt inner_type in
            let opt_datatype =
              mk_option opt_name
                (mk_atom (Format.asprintf "%a" pp_type_ref inner_type))
            in
            add_decl (Ident opt_name) (opt_datatype, mk_atom opt_name)
          | _ -> acc)
      in
      let%bind {decls; _} = get_state () in
      let datatype_name = Format.sprintf "%s_State" name in
      let datatype =
        mk_datatype datatype_name
          (fun mk_constr -> [mk_constr 0])
          (fun mk_accr ->
            List.mapi es ~f:(fun i -> function
              | Bool _ -> mk_accr 0 i sexp_bool
              | Int _ -> mk_accr 0 i sexp_int
              | Float _ -> mk_accr 0 i sexp_real
              | String _ -> mk_accr 0 i sexp_string
              | Null null_type ->
                let inner_type =
                  match Option.value_exn null_type with
                  | OptionType t -> t
                  | _ -> raise Unreachable
                in
                let opt_name = type_ref_to_opt inner_type in
                mk_accr 0 i (mk_atom opt_name)
              | List (_, Some (ListType t)) ->
                mk_accr 0 i (sexp_list (mk_atom (show_type_ref t)))
              | _ -> raise Unreachable))
      in
      let _I_name = Format.sprintf "%s_I" name in
      let _I =
        mk_define_const _I_name datatype_name
          (mk_list
             (get_constr datatype 0
             :: List.map es ~f:(function
               | Bool b -> mk_bool b
               | Int i -> mk_int i
               | Float f -> mk_real f
               | String s -> mk_string s
               | Null null_type ->
                 let inner_type =
                   match Option.value_exn null_type with
                   | OptionType t -> t
                   | _ -> raise Unreachable
                 in
                 let opt_name = type_ref_to_opt inner_type in
                 let opt_datatype, _ = Map.find_exn decls (Ident opt_name) in
                 get_constr opt_datatype 0
               | List _ as l -> fst (fst (encode_expr name l is_rel st))
               | _ -> raise Unreachable)))
      in
      let sexp_datatype_name = mk_atom datatype_name in
      let%bind () =
        add_decl (Ident datatype_name) (datatype, sexp_datatype_name)
      in
      let%bind () = add_decl (Ident _I_name) (_I, sexp_datatype_name) in
      return (datatype, sexp_datatype_name)
    else
      let%bind {ret_type; _} = get_state () in
      match ret_type with
      | Some ret_type when not (is_List ret_type) ->
        let decls =
          List.foldi es ~init:[] ~f:(fun i acc e ->
              let _, field_type = get_typed_accr ret_type 0 i in
              let (), st' = set_ret_type (Some field_type) st in
              let (e_sexp, _), _ = encode_expr name e is_rel st' in
              acc @ [e_sexp])
        in
        return (mk_list decls, sexp_unused)
      | _ ->
        let t = match e with List (_, t) -> t | _ -> None in
        return
          ( mk_List
              (List.map es ~f:(fun e ->
                   fst (fst (encode_expr name e is_rel st)))),
            match t with
            | Some t -> sexp_list (mk_atom (show_type_ref t))
            | None -> sexp_unused ))
  | Proj (e, i) ->
    let%bind e_sexp, e_type = encode_expr name e is_rel in
    if is_List e_type then return (mk_head e_sexp, get_List_type e_type)
    else
      let accr, accr_type = get_typed_accr e_type 0 i in
      return (accr *. e_sexp, accr_type)
  | Tail e ->
    let%bind e_sexp, e_type = encode_expr name e is_rel in
    return (mk_tail e_sexp, e_type)
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
    let%bind e1_sexp, e1_type = encode_expr name e1 is_rel
    and e2_sexp, e2_type = encode_expr name e2 is_rel in
    return
      ( (match e with
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
        | _ -> raise Unreachable)
          e1_sexp e2_sexp,
        match e with
        | Eq _ | Ge _ | Gt _ | Le _ | Lt _ -> sexp_bool
        | _ -> if Sexp.(e1_type = sexp_unused) then e2_type else e1_type )
  | Not e ->
    let%bind e_sexp, e_type = encode_expr name e is_rel in
    return (~.e_sexp, e_type)
  | If (e1, e2, e3) ->
    let%bind e1_sexp, _ = encode_expr name e1 is_rel
    and e2_sexp, e2_type = encode_expr name e2 is_rel
    and e3_sexp, e3_type = encode_expr name e3 is_rel in
    let%bind {decls; ret_type; _} = get_state () in
    let ret_type = Option.value_exn ret_type in
    let e2_sexp, e2_type =
      if is_opt ret_type && Sexp.(get_inner_type ret_type = e2_type) then
        let ret_datatype, _ =
          Map.find_exn decls (Ident (get_atom_str ret_type))
        in
        (get_constr ret_datatype 1 *. e2_sexp, ret_type)
      else (e2_sexp, e2_type)
    in
    let e3_sexp, e3_type =
      if is_opt ret_type && Sexp.(get_inner_type ret_type = e3_type) then
        let ret_datatype, _ =
          Map.find_exn decls (Ident (get_atom_str ret_type))
        in
        (get_constr ret_datatype 1 *. e3_sexp, ret_type)
      else (e3_sexp, e3_type)
    in
    if Sexp.(e2_type <> e3_type) then raise (Compile_error (2, "Type mismatch"));
    return (mk_ite e1_sexp e2_sexp e3_sexp, e2_type)
  | Lambda _ | Fix _ -> raise Unreachable
  | App (f, es) -> (
    match f with
    | Fold ->
      (* incr_ast_size is_rel 4; *)
      let e1, e2, e3 =
        match es with [e1; e2; e3] -> (e1, e2, e3) | _ -> raise Unreachable
      in
      let fix, param1, param2, body =
        match e3 with
        | Lambda ([param1; param2], body) -> (None, param1, param2, body)
        | Fix (Ident fix, [param1; param2], body) ->
          (Some fix, param1, param2, body)
        | _ -> raise Unreachable
      in
      let%bind e1_sexp, e1_type = encode_expr name e1 is_rel
      and e2_sexp, e2_type = encode_expr name e2 is_rel in
      let%bind st = get_state () in
      let e2_datatype, _ =
        Map.find_exn st.decls (Ident (get_atom_str e2_type))
      in
      let f_name =
        Format.sprintf "%s_%s" name (Option.value ~default:"f" fix)
      in
      let%bind () = add_type_sig (Ident f_name) ([e2_sexp; e1_sexp], e2_sexp) in
      let param1_name = mk_atom "a" in
      let param2_name = mk_atom "r" in
      let%bind () = add_decl param1 (param1_name, e2_sexp) in
      let%bind () = add_decl param2 (param2_name, e1_sexp) in
      let%bind () = set_is_type false in
      let%bind () = set_ret_type (Some e2_datatype) in
      let%bind body_sexp, _ = encode_expr name body is_rel in
      let%bind () = set_state st in
      let f =
        mk_define_fun (Option.is_some fix) (mk_atom f_name)
          [param1_name *. e2_type; param2_name *. e1_type]
          e2_type
          (get_constr e2_sexp 0 **. body_sexp)
      in
      let%bind () = add_decl (Ident f_name) (f, e2_type) in
      return (f, e2_sexp)
    | Filter ->
      let e1, e2 =
        match es with [e1; e2] -> (e1, e2) | _ -> raise Unreachable
      in
      let fix, param1, body =
        match e2 with
        | Lambda ([param1], body) -> (None, param1, body)
        | Fix (Ident fix, [param1], body) -> (Some fix, param1, body)
        | _ -> raise Unreachable
      in
      let%bind e1_sexp, e1_datatype = encode_expr name e1 is_rel in
      let e1_type = get_range e1_sexp in
      let _P_name =
        Format.sprintf "%s_%s" name (Option.value ~default:"P" fix)
      in
      let%bind () = add_type_sig (Ident _P_name) ([e1_datatype], sexp_bool) in
      let param1_name = mk_atom "a" in
      let%bind st = get_state () in
      let%bind () = add_decl param1 (param1_name, e1_datatype) in
      let%bind () = set_is_type false in
      let%bind () = set_ret_type (Some sexp_bool) in
      let%bind body_sexp, _ = encode_expr name body false in
      let%bind () = set_state st in
      let _P =
        mk_define_fun (Option.is_some fix) (mk_atom _P_name)
          [param1_name *. e1_type]
          sexp_bool body_sexp
      in
      let%bind () = add_decl (Ident _P_name) (_P, e1_type) in
      return (_P, e1_type)
    | Insert ->
      (* incr_ast_size is_rel 1; *)
      let e1, e2 =
        match es with [e1; e2] -> (e1, e2) | _ -> raise Unreachable
      in
      let%bind e1_sexp, _ = encode_expr name e1 is_rel
      and e2_sexp, e2_type = encode_expr name e2 is_rel in
      return (e1_sexp ++. e2_sexp, e2_type)
    | Other (Ident f) -> (
      (* incr_ast_size is_rel 2; *)
      match f with
      | "len" -> (
        match es with
        | [e] ->
          let%bind e_sexp, e_type = encode_expr name e is_rel in
          if Sexp.(e_type <> sexp_string) then
            raise (Compile_error (2, "Type mismatch"));
          return (sexp_strlen *. e_sexp, sexp_int)
        | _ -> raise Unreachable)
      | _ ->
        raise Unreachable
        (* match es with
      | [e1] ->
        let f_name = Format.sprintf "%s_%s" name f in
        let%bind {type_sigs; _} = get_state () in
        let e1_type, ret_type =
          match Map.find_exn type_sigs (Ident f_name) with
          | [e1_type], ret_type -> (e1_type, ret_type)
          | _ -> raise Unreachable
        in
        let%bind () = set_ret_type (Some e1_type) in
        let%bind e1_sexp, _ = encode_expr name e1 in
        let e1_constr = get_constr e1_type 0 in
        return (mk_list [mk_atom f_name; e1_constr **. e1_sexp], ret_type)
      | [e1; e2] ->
        let f_name = Format.sprintf "%s_%s" name f in
        let%bind {type_sigs; _} = get_state () in
        let (e1_type, e2_type), ret_type =
          match Map.find_exn type_sigs (Ident f_name) with
          | [e1_type; e2_type], ret_type -> ((e1_type, e2_type), ret_type)
          | _ -> raise Unreachable
        in
        let%bind () = set_ret_type (Some e1_type) in
        let%bind e1_sexp, _ = encode_expr name e1 in
        let%bind () = set_ret_type (Some e2_type) in
        let%bind e2_sexp, _ = encode_expr name e2 in
        let e1_constr = get_constr e1_type 0 in
        let e2_constr = get_constr e2_type 0 in
        return
          ( mk_list
              [mk_atom f_name; e1_constr **. e1_sexp; e2_constr **. e2_sexp],
            ret_type )
      | _ -> raise Unreachable *)
      ))
  | Match (e, cases) ->
    (* incr_ast_size is_rel (List.length cases); *)
    let%bind e_sexp, e_type = encode_expr name e is_rel in
    if not (is_opt e_type) then raise (Compile_error (2, "Type mismatch"));
    let%bind {decls; ret_type; _} = get_state () in
    let ret_type = Option.value_exn ret_type in
    let e_inner_type = get_inner_type e_type in
    let e_datatype, _ = Map.find_exn decls (Ident (get_atom_str e_type)) in
    let accr, _ = get_typed_accr e_datatype 1 0 in
    let%bind ite, ite_type =
      List.fold_right cases
        ~init:(return (sexp_unused, sexp_unused))
        ~f:(fun (pat, body) acc ->
          let%bind acc_ite, acc_ite_type = acc in
          let%bind {decls; ret_type; _} = get_state () in
          let ret_type = Option.value_exn ret_type in
          match pat with
          | Null _ ->
            let%bind body, body_type = encode_expr name body is_rel in
            let body, body_type =
              if is_opt ret_type && Sexp.(get_inner_type ret_type = body_type)
              then
                let ret_datatype, _ =
                  Map.find_exn decls (Ident (get_atom_str ret_type))
                in
                (get_constr ret_datatype 1 *. body, ret_type)
              else (body, body_type)
            in
            if Sexp.(body_type <> acc_ite_type) then
              raise (Compile_error (2, "Type mismatch"));
            let recg = get_recg e_datatype 0 in
            return (mk_ite (recg *. e_sexp) body acc_ite, body_type)
          | Var id ->
            let%bind () = add_decl id (accr *. e_sexp, e_inner_type) in
            let%bind body, body_type = encode_expr name body is_rel in
            let body, body_type =
              if is_opt ret_type && Sexp.(get_inner_type ret_type = body_type)
              then
                let ret_datatype, _ =
                  Map.find_exn decls (Ident (get_atom_str ret_type))
                in
                (get_constr ret_datatype 1 *. body, ret_type)
              else (body, body_type)
            in
            return (body, body_type)
          | _ -> raise Unreachable)
    in
    let ite, ite_type =
      if is_opt ret_type && Sexp.(get_inner_type ret_type = ite_type) then
        let ret_datatype, _ =
          Map.find_exn decls (Ident (get_atom_str ret_type))
        in
        (get_constr ret_datatype 1 *. ite, ret_type)
      else (ite, ite_type)
    in
    return (ite, ite_type)

let to_sorted_list decls order =
  order |> List.rev_map ~f:(Map.find_exn decls) |> List.unzip |> fst

let extract_defs decls =
  let _State_def, _State =
    List.find_map_exn decls ~f:(fun (Ident.Ident s, (_State_def, _State)) ->
        Option.some_if (String.is_suffix s ~suffix:"_State") (_State_def, _State))
  in
  let _Row_def, _Row =
    List.find_map_exn decls ~f:(fun (Ident.Ident s, (_Row_def, _Row)) ->
        Option.some_if (String.is_suffix s ~suffix:"_Row") (_Row_def, _Row))
  in
  let f =
    List.find_map_exn decls ~f:(fun (Ident.Ident s, _) ->
        Option.some_if (String.is_suffix s ~suffix:"_f") (mk_atom s))
  in
  let _I =
    List.find_map_exn decls ~f:(fun (Ident.Ident s, _) ->
        Option.some_if (String.is_suffix s ~suffix:"_I") (mk_atom s))
  in
  let _P =
    List.find_map_exn decls ~f:(fun (Ident.Ident s, _) ->
        Option.some_if (String.is_suffix s ~suffix:"_P") (mk_atom s))
  in
  (_State_def, _State, _Row_def, _Row, f, _I, _P)

(* Top-level entry: encode the program into SMT-LIB declarations and return
   the ordered list of declarations plus the extracted signatures. *)
let encode ast =
  let {decls; order; _} =
    match ast with
    | Decl ((Ident s as id), _, expr) ->
      ast_size := !ast_size + 6;
      let decl, st' = encode_expr s expr true init in
      snd (add_decl id decl st')
  in
  (* printf "%d\n" !ast_size;
  exit 0; *)
  (decls, order)
