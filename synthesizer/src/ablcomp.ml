open Core
open Pusharoot
open Solver
open Exns

let in_file = ref ""
let chc_mode = ref None
let _P'1 = ref ""
let _P'2 = ref ""
let _Q1 = ref ""
let _Q2 = ref ""

let () =
  Arg.parse
    [
      ( "-i",
        Arg.String (fun file_path -> in_file := file_path),
        "[file_path] Path to input DSL program" );
      ( "--chc-mode",
        Arg.String (fun solver_id -> chc_mode := Some solver_id),
        "[eld|spacer] Use a CHC solver to infer bisimulation invariant and P'"
      );
      ("-p1", Arg.String (fun p1 -> _P'1 := p1), "P'1 sexp");
      ("-p2", Arg.String (fun p2 -> _P'2 := p2), "P'2 sexp");
      ("-q1", Arg.String (fun q1 -> _Q1 := q1), "Q1 sexp (optional)");
      ("-q2", Arg.String (fun q2 -> _Q2 := q2), "Q2 sexp (optional)");
    ]
    (fun _ -> ())
    "Pusharoo CLI";
  let ast =
    In_channel.read_all !in_file
    |> Lexing.from_string |> Parser.start Lexer.token |> Pass_reduce.reduce
    |> Pass_gc.gc |> List.hd_exn
  in
  let decls, order = Pass_encode.encode ast in
  let udaf = Pass_encode.to_sorted_list decls order in
  let decls = Map.to_alist decls in
  let _State_def, _State, _Row_def, _Row, f, _I, _P =
    Pass_encode.extract_defs decls
  in
  Utils.m :=
    {
      !Utils.m with
      d = -1;
      save = false;
      decls;
      udaf;
      _State_def;
      _State;
      _Row_def;
      _Row;
      f;
      _I;
      _P;
    };
  let filename = Filename.chop_extension (Filename.basename !in_file) in
  let _P'1 = Sexp.of_string !_P'1 in
  let _P'2 = Sexp.of_string !_P'2 in
  let compP'12 = Synth.gen_P'_comp_kind false _P'1 _P'2 in
  let compP'21 = Synth.gen_P'_comp_kind true _P'2 _P'1 in
  let compP' =
    match
      ( not (try (solve compP'12).sat with Solver_unknown -> true),
        not (try (solve compP'21).sat with Solver_unknown -> true) )
    with
    | true, true -> 0
    | true, false -> 1
    | false, true -> 2
    | false, false -> 3
  in
  if String.(!_Q1 <> "" && !_Q2 <> "") then
    let _Q1 = Sexp.of_string !_Q1 in
    let _Q2 = Sexp.of_string !_Q2 in
    let compQ12 = Synth.gen_Q_comp _Q1 _Q2 in
    let compQ21 = Synth.gen_Q_comp _Q2 _Q1 in
    let compQ =
      match
        ( not (try (solve compQ12).sat with Solver_unknown -> true),
          not (try (solve compQ21).sat with Solver_unknown -> true) )
      with
      | true, true -> 0
      | true, false -> 1
      | false, true -> 2
      | false, false -> 3
    in
    printf "%s,%d,%d\n" filename compQ compP'
  else printf "%s,%d\n" filename compP'
