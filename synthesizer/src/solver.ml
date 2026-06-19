open Core
open Utils
open Exns

let escape_smtlib_string_literals s =
  let len = String.length s in
  let buf = Buffer.create (len + 16) in
  let i = ref 0 in
  while !i < len do
    if Char.equal (String.get s !i) Smtlib_utils.dq_char then (
      (* Found opening double-quote; scan to closing double-quote *)
      let j = ref (!i + 1) in
      while
        !j < len && not (Char.equal (String.get s !j) Smtlib_utils.dq_char)
      do
        incr j
      done;
      (* inner = content between the quotes *)
      let inner = String.sub s ~pos:(!i + 1) ~len:(!j - !i - 1) in
      (* Emit as sexp quoted atom with literal quotes in payload:
         "\"<inner>\"" which Sexp.of_string parses as Atom {|"<inner>"|} *)
      Buffer.add_char buf Smtlib_utils.dq_char;
      Buffer.add_char buf '\\';
      Buffer.add_char buf Smtlib_utils.dq_char;
      Buffer.add_string buf inner;
      Buffer.add_char buf '\\';
      Buffer.add_char buf Smtlib_utils.dq_char;
      Buffer.add_char buf Smtlib_utils.dq_char;
      i := !j + 1)
    else (
      Buffer.add_char buf (String.get s !i);
      incr i)
  done;
  Buffer.contents buf

type solution = {
  sat : bool;
  unsat_core : int;
  model : (string * Sexp.t) list;
  error : string;
}

let pp_solution fmt sol =
  fprintf fmt "[%s|%s|%s]"
    (if sol.sat then "sat" else "unsat")
    (sprintf "VC%d" sol.unsat_core)
    (sol.model
    |> List.map ~f:(fun (k, _) -> sprintf "%s" k)
    |> String.concat ~sep:",")

let print_solution = printf "%a\n" pp_solution

(* Tail of a string, as a list of lines, capped at [max_lines].  Used to keep
   solver error reports short but still informative. *)
let tail_lines ~max_lines s =
  let lines = String.split s ~on:'\n' in
  let n = List.length lines in
  if n <= max_lines then lines else List.drop lines (n - max_lines)

let solver_diagnostic ~status ~stdout_tail ~stderr_tail =
  let exe = !m.solver in
  let status_str =
    match status with
    | Caml_unix.WEXITED i -> sprintf "exit %d" i
    | Caml_unix.WSIGNALED s ->
      let hint =
        if s = Stdlib.Sys.sigill then
          " (SIGILL: host CPU may lack instructions required by this solver \
           build; try running the Docker image on a different machine)"
        else ""
      in
      sprintf "killed by signal %d%s" s hint
    | Caml_unix.WSTOPPED s -> sprintf "stopped by signal %d" s
  in
  let tail label lines =
    match lines with
    | [] -> ""
    | _ ->
      sprintf "\n--- %s (last %d lines) ---\n%s" label (List.length lines)
        (String.concat ~sep:"\n" lines)
  in
  sprintf "solver %s failed: %s%s%s" exe status_str
    (tail "stdout" stdout_tail)
    (tail "stderr" stderr_tail)

let solve encoding =
  let stdout_ic, stdin_oc, stderr_ic =
    Caml_unix.open_process_args_full !m.solver !m.solver_args
      (Caml_unix.environment ())
  in
  Out_channel.output_string stdin_oc encoding;
  Out_channel.flush stdin_oc;
  Out_channel.close stdin_oc;
  let buf = Buffer.create 1000 in
  (try
     while true do
       Buffer.add_string buf (In_channel.input_line_exn stdout_ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  (* Drain stderr too: without this, solver failures like CVC5's "Illegal
     instruction" on older CPUs or an Eldarica wrapper crash are invisible. *)
  let errbuf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string errbuf (In_channel.input_line_exn stderr_ic);
       Buffer.add_char errbuf '\n'
     done
   with End_of_file -> ());
  let init_sol = {sat = false; unsat_core = -1; model = []; error = ""} in
  let status = Caml_unix.close_process_full (stdout_ic, stdin_oc, stderr_ic) in
  match status with
  | Caml_unix.WEXITED i when i = 0 || i = 1 ->
    List.fold
      (String.split (Buffer.contents buf) ~on:'\n')
      ~init:(init_sol, None)
      ~f:(fun (sol, buf) l ->
        (* if String.(exe = "z3") then printf "%s\n%!" l; *)
        match l with
        | "(" -> (sol, Some "(")
        | ")" -> (
          let raw_model = Option.value_exn buf ^ ")" in
          let escaped_model = escape_smtlib_string_literals raw_model in
          match Sexp.of_string escaped_model with
          | Sexp.List defs ->
            ( {
                sol with
                model =
                  List.map defs ~f:(function
                    | Sexp.List
                        (Sexp.Atom "define-fun" :: Sexp.Atom def_name :: _) as
                      def ->
                      (def_name, def)
                    | _ -> raise Unreachable);
              },
              None )
          | Sexp.Atom _ -> raise Unreachable)
        | "sat" -> ({sol with sat = true}, buf)
        | "unknown" ->
          (* printf "unknown\n"; *)
          raise Solver_unknown
        | _ when String.is_prefix l ~prefix:"(VC" ->
          let unsat_core =
            l
            |> String.unsafe_sub ~pos:1 ~len:(String.length l - 2)
            |> String.split ~on:'_'
            (* in
          if List.length unsat_core <> 2 then raise Solver_unknown;
          let unsat_core = unsat_core *)
            |> List.last_exn
            |> Int.of_string
          in
          ({sol with unsat_core}, buf)
        | _ when String.is_prefix l ~prefix:"(error" ->
          let error = String.unsafe_sub l ~pos:8 ~len:(String.length l - 10) in
          (* "...not available..." is the benign case where the solver
             declines to produce a model/unsat-core for a particular query;
             any other error indicates a real problem and should surface. *)
          if String.is_substring error ~substring:"avail" then (sol, buf)
          else raise (Solver_error (sprintf "%s reported: %s" !m.solver error))
        | _ when Option.is_some buf ->
          (sol, Some (Option.value_exn buf ^ "\n" ^ l))
        | _ -> (sol, buf))
    |> fst
  | _ ->
    raise
      (Solver_error
         (solver_diagnostic ~status
            ~stdout_tail:(tail_lines ~max_lines:20 (Buffer.contents buf))
            ~stderr_tail:(tail_lines ~max_lines:20 (Buffer.contents errbuf))))
