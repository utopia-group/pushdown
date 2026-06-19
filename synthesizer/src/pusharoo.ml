open Core
open Pusharoot
open Utils
open Smtlib_utils
open Exns

let in_file = ref ""
let timeout = ref 600.
let save_qp = ref ""
let ast_size_only = ref false
let udf_props_only = ref false

let () =
  Arg.parse
    [
      ( "--solver",
        Arg.String
          (fun solver_id ->
            m :=
              match solver_id with
              | "z3" -> {!m with solver = "z3"; solver_args = z3_args}
              | "cvc5" -> {!m with solver = "cvc5"; solver_args = cvc5_args}
              | _ ->
                raise
                  (Config_error (sprintf "Unknown backend solver %s" solver_id))),
        "[z3|cvc5] Backend solver: Z3 (default) or CVC5" );
      ( "--save",
        Arg.Bool (fun s -> m := {!m with save = s}),
        "Save SMT-LIB encodings incurred during synthesis to files" );
      ( "--debug",
        Arg.Int (fun level -> m := {!m with d = level}),
        {|[-1|0|1|2]
      -1 (default): no debug output
      0: synthesizer debug messages
      1: 0 + AST
      2: 1 + declarations and ordering |}
      );
      ( "--timeout",
        Arg.Float (fun t -> timeout := t),
        "Timeout in seconds (default: 600)" );
      ( "--mode",
        Arg.String
          (function
          | "pusharoo" -> m := {!m with mode = Pusharoo}
          | "nobounds" -> m := {!m with mode = Abl_nobounds}
          | "noanalysis" -> m := {!m with mode = Abl_noanalysis}
          | "nobounds_noanalysis" ->
            m := {!m with mode = Abl_nobounds_noanalysis}
          | "twophase" -> m := {!m with mode = Abl_twophase}
          | "spacer" ->
            m :=
              {!m with mode = Abl_spacer; solver = "z3"; solver_args = z3_args}
          | "eldarica" ->
            m :=
              {
                !m with
                mode = Abl_eldarica;
                solver = "eld";
                solver_args = eld_args;
              }
          | s -> raise (Config_error (sprintf "Unknown mode %s" s))),
        "Specify synthesis mode" );
      ( "--save-qp",
        Arg.String (fun f -> save_qp := f),
        "Append name,Q*,P'* to given CSV file" );
      ("--ast-size", Arg.Set ast_size_only, "Print UDF AST size and exit");
      ( "--udf-props",
        Arg.Set udf_props_only,
        "Print UDF boolean properties and exit" );
    ]
    (fun file_path -> in_file := file_path)
    "Pusharoo CLI";
  let filename = Filename.chop_extension (Filename.basename !in_file) in
  (* printf "%s," filename; *)
  ignore
    (Core_unix.setitimer ITIMER_REAL {it_interval = 0.; it_value = !timeout});
  Signal.Expert.handle Signal.alrm (fun _ ->
      printf "%s,timeout,,,,,,,%d,,\n" filename (Hash_set.length Synth.pairs);
      exit 0);
  let raw = In_channel.read_all !in_file in
  if !ast_size_only then (
    let ast =
      raw |> Lexing.from_string |> Parser.start Lexer.token
      |> Pass_reduce.reduce |> Pass_gc.gc |> List.hd_exn
    in
    printf "%s,%d\n" filename (Ast.size_of_stmt ast);
    exit 0);
  if !udf_props_only then (
    let ast =
      raw |> Lexing.from_string |> Parser.start Lexer.token
      |> Pass_reduce.reduce |> Pass_gc.gc |> List.hd_exn
    in
    (* Extract fold body and init from: Decl(_, _, App(Filter, [App(Fold, [_; init; Lambda([a; _], body)]); _])) *)
    let has_conditional =
      let rec has_cond = function
        | Ast.If _ -> true
        | Ast.Match (_, cases) ->
          List.exists cases ~f:(fun (_, e) -> has_cond e)
        | Ast.Add (e1, e2)
        | Ast.Sub (e1, e2)
        | Ast.Mul (e1, e2)
        | Ast.Div (e1, e2)
        | Ast.Eq (e1, e2)
        | Ast.Ge (e1, e2)
        | Ast.Gt (e1, e2)
        | Ast.Le (e1, e2)
        | Ast.Lt (e1, e2)
        | Ast.And (e1, e2)
        | Ast.Or (e1, e2) ->
          has_cond e1 || has_cond e2
        | Ast.Not e | Ast.Proj (e, _) | Ast.Tail e -> has_cond e
        | Ast.Tuple es | Ast.List (es, _) | Ast.App (_, es) ->
          List.exists es ~f:has_cond
        | Ast.Lambda (_, body) | Ast.Fix (_, _, body) -> has_cond body
        | _ -> false
      in
      match ast with
      | Ast.Decl
          ( _,
            _,
            Ast.App
              ( Ast.Filter,
                [Ast.App (Ast.Fold, [_; _; Ast.Lambda ([_; _], body)]); _] ) )
        ->
        has_cond body
      | _ -> false
    in
    let has_tuple_accum =
      match ast with
      | Ast.Decl
          ( _,
            _,
            Ast.App (Ast.Filter, [Ast.App (Ast.Fold, [_; Ast.Tuple es; _]); _])
          )
      | Ast.Decl
          ( _,
            _,
            Ast.App
              (Ast.Filter, [Ast.App (Ast.Fold, [_; Ast.List (es, _); _]); _]) )
        ->
        List.length es > 1
      | _ -> false
    in
    (* Compute CFG-based properties *)
    let cfg, _rev_cfg = Pass_cfa.analyze ast in
    let has_indep =
      Map.length cfg > 1
      && cfg |> Map.to_alist
         |> List.exists ~f:(fun ((a, i), targets) ->
             (not
                (Set.exists targets ~f:(function
                  | (Ast.Ident.Ident "a", i'), _ -> i <> i'
                  | _ -> false)))
             && not
                  (Set.exists
                     (Map.find_exn _rev_cfg (a, i))
                     ~f:(function
                       | Ast.Ident.Ident "a", i' -> i <> i' | _ -> false)))
    in
    let has_crossdep =
      _rev_cfg |> Map.to_alist
      |> List.exists ~f:(function
        | (Ast.Ident.Ident "a", i), targets ->
          Set.exists targets ~f:(function
            | Ast.Ident.Ident "a", i' -> i <> i'
            | _ -> false)
        | _ -> false)
    in
    printf "%s,%b,%b,%b,%b\n" filename has_conditional has_tuple_accum has_indep
      has_crossdep;
    exit 0);
  let t0 = Core.Time_ns.now () in
  let sol, _P_comps, num_pairs =
    try Synth.synth raw
    with Exns.Solver_unknown ->
      (* printf "unknown\n"; *)
      (None, Set.empty (module Sexp), 0)
  in
  let t1 = Core.Time_ns.now () in
  let runtime = Core.Time_ns.Span.to_sec (Core.Time_ns.diff t1 t0) in
  match sol with
  | None -> printf "%s,none\n" filename
  | Some (_Q, _BI, _P') ->
    let kind = Synth.get_kind _Q _BI _P' in
    if kind = -1 then printf "%s,none\n" filename
    else (
      if !m.d >= 0 then (
        printf "Q:\n%s\n%!" (Sexp.to_string_hum _Q);
        printf "P':\n%s\n%!" (Sexp.to_string_hum _P')
        (* printf "BI:\n%s\n" (Sexp.to_string_hum _BI) *));
      if String.(!save_qp <> "") then (
        let oc = Out_channel.create ~append:true !save_qp in
        fprintf oc "%s,%s,%s\n" filename (Sexp.to_string_mach _Q)
          (Sexp.to_string_mach _P');
        Out_channel.close oc);
      let _P'_size =
        if kind = 1 then 0
        else if is_abl_spacer () || is_abl_eldarica () then list_length _P'
        else list_length _P' - 1
      in
      (* printf "%s,%s,%s\n" filename (Sexp.to_string_mach _Q)
        (Sexp.to_string_mach _P') *)
      let magicpush_exact, magicpush_partial =
        if is_pusharoo () then
          _Q |> Synth.magicpush_possible |> Tuple2.map ~f:Bool.to_string
        else ("", "")
      in
      (* printf "pairs:\n%s\n"
        (Synth.pairs |> Hash_set.to_list
        |> List.map ~f:(fun (p1, p2) ->
               let p1 = p1 |> Set.to_list |> mk_and in
               let p2 = p2 |> Set.to_list |> mk_and in
               sprintf "(%s,%s)" (Sexp.to_string_mach p1)
                 (Sexp.to_string_mach p2))
        |> String.concat ~sep:"\n"); *)
      (* benchmark,kind,runtime,|Q*|,|P*|,|BI|,|P|,|DP|,num_pairs,magicpush_exact,magicpush_partial*)
      printf "%s,%d,%.2f,%d,%d,%d,%d,%d,%d,%s,%s\n" filename kind runtime
        (list_length _Q) _P'_size (list_length _BI) (Set.length _P_comps)
        (Set.length
           (Set.diff _P_comps (Set.of_list (module Sexp) (list_split _P'))))
        num_pairs magicpush_exact magicpush_partial)
