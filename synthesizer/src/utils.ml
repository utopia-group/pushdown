open Core

type mode =
  | Pusharoo
  | Abl_nobounds
  | Abl_noanalysis
  | Abl_nobounds_noanalysis
  | Abl_twophase
  | Abl_spacer
  | Abl_eldarica
  | Inferall
[@@deriving variants]

type state = {
  d : int;
  save : bool;
  iters : int;
  mode : mode;
  solver : string;
  solver_args : string array;
  decls : (Ast.Ident.t * (Sexp.t * Sexp.t)) list;
  udaf : Sexp.t list;
  _State_def : Sexp.t;
  _State : Sexp.t;
  _Row_def : Sexp.t;
  _Row : Sexp.t;
  f : Sexp.t;
  _I : Sexp.t;
  _P : Sexp.t;
  _BI_min : Sexp.t;
  _P_comps : Set.M(Sexp).t;
}

let z3_args = [| "-smt2"; "-T:10"; "-in" |]
let eld_args = [| "-hsmt"; "-horn"; "-ssol"; "-t:10"; "-in" |]
let cvc5_args = [| "--lang=smt2"; "--produce-models"; "--no-interactive"; "-" |]
let sexp_unused = Sexp.Atom "_"

let m =
  ref
    {
      d = -1;
      save = false;
      iters = 0;
      mode = Pusharoo;
      solver = "z3";
      solver_args = z3_args;
      decls = [];
      udaf = [];
      _State_def = sexp_unused;
      _State = sexp_unused;
      _Row_def = sexp_unused;
      _Row = sexp_unused;
      f = sexp_unused;
      _I = sexp_unused;
      _P = sexp_unused;
      _BI_min = sexp_unused;
      _P_comps = Set.empty (module Sexp);
    }

let is_pusharoo () = is_pusharoo !m.mode

let is_abl_nobounds () =
  is_abl_nobounds !m.mode || is_abl_nobounds_noanalysis !m.mode

let is_abl_noanalysis () =
  is_abl_noanalysis !m.mode || is_abl_nobounds_noanalysis !m.mode

let is_abl_twophase () = is_abl_twophase !m.mode
let is_abl_spacer () = is_abl_spacer !m.mode
let is_abl_eldarica () = is_abl_eldarica !m.mode
