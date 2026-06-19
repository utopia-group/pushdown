open Core
open Ke
open Solver
open Utils
open Smtlib_utils
open Exns

let save_encoding save filename encoding =
  if save then (
    let out_file = sprintf "%s.smt2" filename in
    let out = Out_channel.create out_file in
    Out_channel.output_string out encoding;
    Out_channel.close out;
    printf "Saved encoding to %s\n%!" out_file)

let prelude unsat_cores minimize =
  (if unsat_cores then
     [mk_list [sexp_set_option; mk_atom ":produce-unsat-cores"; sexp_true]]
   else [])
  @
  if minimize then
    [mk_list [sexp_set_option; mk_atom ":smt.core.minimize"; sexp_true]]
  else []

let postlude get_unsat_core get_model =
  mk_unary (mk_atom "check-sat")
  :: (if get_unsat_core then [mk_unary (mk_atom "get-unsat-core")] else [])
  @ if get_model then [mk_unary (mk_atom "get-model")] else []

let _U_to_string _U =
  _U |> List.map ~f:(fun u -> sexp_to_smtlib_string u) |> String.concat

let gen_Q _U_Q =
  List.mapi _U_Q ~f:(fun i cand ->
      mk_define_fun false
        (mk_atom (sprintf "Q_%d" i))
        [mk_atom "r" *. !m._Row]
        sexp_bool cand)

let gen_Q_one name u =
  mk_define_fun false name [mk_atom "r" *. !m._Row] sexp_bool u

let gen_Q_all name _U_Q =
  mk_define_fun false name [mk_atom "r" *. !m._Row] sexp_bool (mk_and _U_Q)

let gen_P' _U_P' =
  let a = mk_atom "a" in
  List.mapi _U_P' ~f:(fun i cand ->
      mk_define_fun false
        (mk_atom (sprintf "P_%d" i))
        [a *. !m._State]
        sexp_bool cand)
  @ [
      mk_define_fun false (mk_atom "P_")
        [a *. !m._State]
        sexp_bool
        (mk_and
           (List.mapi _U_P' ~f:(fun i _ -> mk_atom (sprintf "P_%d" i) *. a)));
    ]

let gen_P'_one name u =
  mk_define_fun false name [mk_atom "a" *. !m._State] sexp_bool u

let gen_P'_all name _U_P' =
  mk_define_fun false name [mk_atom "a" *. !m._State] sexp_bool (mk_and _U_P')

let gen_Q_comp ?(op = ( =>. )) _Q1_def _Q2_def =
  let r = mk_atom "r" in
  let _Q1 = mk_atom "Q1" in
  let _Q2 = mk_atom "Q2" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_one _Q1 _Q1_def;
      gen_Q_one _Q2 _Q2_def;
      mk_declare_const r !m._Row;
      mk_assert ~.(op (_Q1 *. r) (_Q2 *. r));
    ]
  @ postlude false false
  |> _U_to_string

let gen_P'_comp _P'1_def _P'2_def =
  let a = mk_atom "a" in
  let _P'1 = mk_atom "P1_" in
  let _P'2 = mk_atom "P2_" in
  prelude false false @ !m.udaf
  @ [
      gen_P'_one _P'1 _P'1_def;
      gen_P'_one _P'2 _P'2_def;
      mk_declare_const a !m._State;
      mk_assert ~.(_P'1 *. a =>. _P'2 *. a);
    ]
  @ postlude false false
  |> _U_to_string

let gen_P'_comp_kind ?(op = ( =>. )) backward _P'1_def _P'2_def =
  let is_eldarica = is_abl_eldarica () in
  let is_spacer = is_abl_spacer () in
  let a = mk_atom "a" in
  let _P'1 = mk_atom "P1_" in
  let _P'2 = mk_atom "P2_" in
  prelude false false @ !m.udaf
  @ [
      mk_define_fun false _P'1
        [
          mk_atom
            (if is_eldarica && backward then "A"
             else if is_spacer && backward then "x!0"
             else "a")
          *. !m._State;
        ]
        sexp_bool _P'1_def;
      mk_define_fun false _P'2
        [
          mk_atom
            (if is_eldarica && not backward then "A"
             else if is_spacer && not backward then "x!0"
             else "a")
          *. !m._State;
        ]
        sexp_bool _P'2_def;
      mk_declare_const a !m._State;
      mk_assert ~.(op (_P'1 *. a) (_P'2 *. a));
    ]
  @ postlude false false
  |> _U_to_string

module U_Q = struct
  type t = Sexp.t list

  let compare (_U1 : t) (_U2 : t) =
    let _U1_conj = mk_and _U1 in
    let _U2_conj = mk_and _U2 in
    let encoding12 = gen_Q_comp _U1_conj _U2_conj in
    let encoding21 = gen_Q_comp _U2_conj _U1_conj in
    let sol12 = solve encoding12 in
    let sol21 = solve encoding21 in
    if Bool.(sol12.sat <> sol21.sat) then if sol12.sat then -1 else 1
    else
      let _U1_len = List.length _U1 in
      let _U2_len = List.length _U2 in
      if _U1_len > _U2_len then -1 else if _U1_len < _U2_len then 1 else 0
end

module U_P' = struct
  type t = Sexp.t list

  let compare (_U1 : t) (_U2 : t) =
    let _U1_conj = mk_and _U1 in
    let _U2_conj = mk_and _U2 in
    let encoding12 = gen_P'_comp _U1_conj _U2_conj in
    let encoding21 = gen_P'_comp _U2_conj _U1_conj in
    let sol12 = solve encoding12 in
    let sol21 = solve encoding21 in
    if Bool.(sol12.sat <> sol21.sat) then if sol12.sat then -1 else 1
    else
      let _U1_len = List.length _U1 in
      let _U2_len = List.length _U2 in
      if _U1_len < _U2_len then -1
      else if _U1_len > _U2_len then 1
      else
        let _U1_size_atoms, _U1_size_chars = Sexp.size _U1_conj in
        let _U2_size_atoms, _U2_size_chars = Sexp.size _U2_conj in
        if _U2_size_atoms < _U1_size_atoms then -1
        else if _U2_size_atoms > _U1_size_atoms then 1
        else if _U1_size_chars < _U2_size_chars then -1
        else if _U1_size_chars > _U2_size_chars then 1
        else 0
end

module QHeap = Batteries.Heap.Make (U_Q)

module UU_P' = struct
  type t = U_P'.t * U_P'.t

  let compare (_U11, _) (_U21, _) = U_P'.compare _U11 _U21
end

module P'Heap = Batteries.Heap.Make (UU_P')

module SexpSet = struct
  module T = struct
    type t = Set.M(Sexp).t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module SexpSetPair = struct
  module T = struct
    type t = Set.M(Sexp).t * Set.M(Sexp).t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

let pairs = Hash_set.create (module SexpSetPair)

let gen_BI name _U_BI =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  List.mapi _U_BI ~f:(fun i cand ->
      mk_define_fun false
        (mk_atom (sprintf "%s_%d" name i))
        [a1 *. !m._State; a2 *. !m._State]
        sexp_bool cand)
  @ [
      mk_define_fun false (mk_atom name)
        [a1 *. !m._State; a2 *. !m._State]
        sexp_bool
        (mk_and
           (List.mapi _U_BI ~f:(fun i _ ->
                mk_list [mk_atom (sprintf "%s_%d" name i); a1; a2])));
    ]

let gen_BI_all name _U_BI =
  mk_define_fun false name
    [mk_atom "a1" *. !m._State; mk_atom "a2" *. !m._State]
    sexp_bool (mk_and _U_BI)

let gen_BI_one name u =
  mk_define_fun false name
    [mk_atom "a1" *. !m._State; mk_atom "a2" *. !m._State]
    sexp_bool u

let gen_magicpush_preconds_no_Q () =
  let r1 = mk_atom "r1" in
  let r2 = mk_atom "r2" in
  let r = mk_atom "r" in
  let a1 = mk_list [!m.f; !m._I; r1] in
  let a2 = mk_list [!m.f; !m._I; r2] in
  let a12 = mk_list [!m.f; a1; r2] in
  prelude false false @ !m.udaf
  @ [
      mk_declare_const r1 !m._Row;
      mk_declare_const r2 !m._Row;
      mk_assert
        ~.(mk_and
             [
               a12 =. mk_list [!m.f; a2; r1];
               [r *. !m._Row] =|. (a12 =. mk_list [!m.f; !m._I; r]);
               ~.(!m._P *. a12) =. (~.(!m._P *. a1) &&. ~.(!m._P *. a2));
             ]);
    ]
  @ postlude false false
  |> _U_to_string

let gen_magicpush_preconds_Q ?(partial = false) _Q_def =
  let r1 = mk_atom "r1" in
  let r2 = mk_atom "r2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let a1 = mk_list [!m.f; !m._I; r1] in
  let a2 = mk_list [!m.f; !m._I; r2] in
  let a12 = mk_list [!m.f; a1; r2] in
  [mk_list [mk_atom "set-logic"; mk_atom "ALL"]]
  @ prelude false false @ !m.udaf
  @ [
      gen_Q_one _Q _Q_def;
      mk_declare_const r1 !m._Row;
      mk_declare_const r2 !m._Row;
      mk_assert
        ~.(mk_and
             [
               a12 =. mk_list [!m.f; a2; r1];
               [r *. !m._Row] =|. (a12 =. mk_list [!m.f; !m._I; r]);
               (if not partial then
                  ~.(!m._P *. a12) =. (~.(!m._P *. a1) &&. ~.(!m._P *. a2))
                else ~.(!m._P *. a12) &&. ~.(_Q *. r2) =>. ~.(!m._P *. a1));
               !m._P *. a12 &&. ~.(_Q *. r2) =>. (a12 =. a1);
             ]);
    ]
  @ postlude false false
  |> _U_to_string

let gen_infer_BI_P' _U_Q =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI = mk_atom "BI" in
  let _P' = mk_atom "P_" in
  let is_spacer = is_abl_spacer () in
  (if is_spacer then [mk_list [mk_atom "set-logic"; mk_atom "HORN"]] else [])
  @ prelude false false @ !m.udaf
  @ [
      (if is_inferall !m.mode then mk_declare_fun "Q" [!m._Row] sexp_bool
       else gen_Q_all _Q _U_Q);
      mk_declare_fun "BI" [!m._State; !m._State] sexp_bool;
      mk_declare_fun "P_" [!m._State] sexp_bool;
      mk_assert (mk_list [_BI; !m._I; !m._I]);
      mk_assert
        ([a1 *. !m._State; a2 *. !m._State; r *. !m._Row]
        |. (mk_list [_BI; a1; a2]
           &&. _Q *. r
           =>. mk_list [_BI; mk_list [!m.f; a1; r]; mk_list [!m.f; a2; r]]));
      mk_assert
        ([a1 *. !m._State; a2 *. !m._State; r *. !m._Row]
        |. (mk_list [_BI; a1; a2]
           &&. ~.(_Q *. r)
           =>. mk_list [_BI; mk_list [!m.f; a1; r]; a2]));
    ]
  @ (if is_spacer then
       [
         mk_assert
           ([a1 *. !m._State; a2 *. !m._State]
           |. (mk_list [_BI; a1; a2] &&. !m._P *. a1 =>. _P' *. a2));
         mk_assert
           ([a1 *. !m._State; a2 *. !m._State]
           |. (mk_list [_BI; a1; a2] &&. !m._P *. a1 =>. (a1 =. a2)));
         mk_assert
           ([a1 *. !m._State; a2 *. !m._State]
           |. (mk_list [_BI; a1; a2] &&. ~.(!m._P *. a1) =>. ~.(_P' *. a2)));
       ]
     else
       [
         mk_assert
           ([a1 *. !m._State; a2 *. !m._State]
           |. (mk_list [_BI; a1; a2]
              =>. (mk_and [!m._P *. a1; _P' *. a2; a1 =. a2]
                  ||. (~.(!m._P *. a1) &&. ~.(_P' *. a2)))));
       ])
  @ postlude false is_spacer
  |> _U_to_string

let gen_all _Q_def _BI_def _P'_def =
  let is_eldarica = is_abl_eldarica () in
  let is_spacer = is_abl_spacer () in
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI = mk_atom "BI" in
  let _P' = mk_atom "P_" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_one _Q _Q_def;
      mk_define_fun false _BI
        [
          mk_atom
            (if is_eldarica then "A" else if is_spacer then "x!0" else "a1")
          *. !m._State;
          mk_atom
            (if is_eldarica then "B" else if is_spacer then "x!1" else "a2")
          *. !m._State;
        ]
        sexp_bool _BI_def;
      mk_define_fun false _P'
        [
          mk_atom (if is_eldarica then "A" else if is_spacer then "x!0" else "a")
          *. !m._State;
        ]
        sexp_bool _P'_def;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_declare_const r !m._Row;
      mk_assert
        ~.(mk_and
             [
               mk_list [_BI; !m._I; !m._I];
               mk_list [_BI; a1; a2]
               &&. _Q *. r
               =>. mk_list [_BI; mk_list [!m.f; a1; r]; mk_list [!m.f; a2; r]];
               mk_list [_BI; a1; a2]
               &&. ~.(_Q *. r)
               =>. mk_list [_BI; mk_list [!m.f; a1; r]; a2];
               mk_list [_BI; a1; a2]
               =>. (mk_and [!m._P *. a1; _P' *. a2; a1 =. a2]
                   ||. (~.(!m._P *. a1) &&. ~.(_P' *. a2)));
             ]);
    ]
  @ postlude false false
  |> _U_to_string

let gen_BI_comp _U_BI_max =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI_min = mk_atom "BI_min" in
  let _BI_max = mk_atom "BI_max" in
  prelude false false @ !m.udaf
  @ [
      gen_BI_one _BI_min !m._BI_min;
      gen_BI_all _BI_max _U_BI_max;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_assert ~.(mk_list [_BI_max; a1; a2] =>. mk_list [_BI_min; a1; a2]);
    ]
  @ postlude false false
  |> _U_to_string

let gen_VC1_one u =
  let _BI = mk_atom "BI" in
  prelude false false @ !m.udaf
  @ [gen_BI_one _BI u; mk_assert (mk_list [_BI; !m._I; !m._I])]
  @ postlude false false
  |> _U_to_string

let gen_VC4_distinguish _U_BI _U_P' =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI = mk_atom "BI" in
  let _P' = mk_atom "P_" in
  prelude true false @ !m.udaf
  @ [
      gen_BI_all _BI _U_BI;
      gen_P'_all _P' _U_P';
      mk_named_assert "VC4_1"
        ([a1 *. !m._State; a2 *. !m._State]
        |. (mk_list [_BI; a1; a2] &&. !m._P *. a1 =>. (_P' *. a2 &&. (a1 =. a2)))
        );
      mk_named_assert "VC4_2"
        ([a1 *. !m._State; a2 *. !m._State]
        |. (mk_list [_BI; a1; a2] &&. ~.(!m._P *. a1) =>. ~.(_P' *. a2)));
    ]
  @ postlude true false
  |> _U_to_string

let gen_VC4_P_cex get_model _U_BI =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI = mk_atom "BI" in
  prelude false false @ !m.udaf
  @ [
      gen_BI_all _BI _U_BI;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_assert
        ~.(mk_list [_BI; a1; a2]
          =>. (mk_and [!m._P *. a1; !m._P *. a2; a1 =. a2]
              ||. (~.(!m._P *. a1) &&. ~.(!m._P *. a2))));
    ]
  @ postlude false get_model
  |> _U_to_string

let gen_VC4_sub_cex _U_BI _U_P' =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI = mk_atom "BI" in
  let _P' = mk_atom "P_" in
  prelude false false @ !m.udaf
  @ [
      gen_BI_all _BI _U_BI;
      gen_P'_all _P' _U_P';
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_assert ~.(mk_list [_BI; a1; a2] &&. ~.(!m._P *. a1) =>. ~.(_P' *. a2));
    ]
  @ postlude false true
  |> _U_to_string

let gen_VC4_one u =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI = mk_atom "BI" in
  prelude false false @ !m.udaf
  @ [
      gen_BI_one _BI u;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_assert
        ~.(mk_and [!m._P *. a1; !m._P *. a2; a1 =. a2]
          ||. (~.(!m._P *. a1) &&. ~.(!m._P *. a2))
          =>. mk_list [_BI; a1; a2]);
    ]
  @ postlude false false
  |> _U_to_string

let gen_Q_select _U_Q (fwd, r) =
  prelude true false @ !m.udaf @ gen_Q _U_Q @ [r]
  @ List.mapi _U_Q ~f:(fun i _ ->
      mk_named_assert (sprintf "VC_%d" i)
        ((if fwd then ( ~. ) else Fn.id)
           (mk_atom (sprintf "Q_%d" i) *. mk_atom "r")))
  @ postlude true false
  |> _U_to_string

let gen_Q_select_one u (fwd, r) =
  let _Q = mk_atom "Q" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_one _Q u;
      r;
      mk_assert ((if fwd then ( ~. ) else Fn.id) (_Q *. mk_atom "r"));
    ]
  @ postlude false false
  |> _U_to_string

let gen_BI_select _U_BI a1 a2 =
  prelude true false @ !m.udaf @ gen_BI "BI" _U_BI @ [a1; a2]
  @ List.mapi _U_BI ~f:(fun i _ ->
      mk_named_assert (sprintf "VC_%d" i)
        (mk_list [mk_atom (sprintf "BI_%d" i); mk_atom "a1"; mk_atom "a2"]))
  @ postlude true false
  |> _U_to_string

let gen_P'_select _U_P' a2 =
  prelude true false @ !m.udaf @ gen_P' _U_P' @ [a2]
  @ List.mapi _U_P' ~f:(fun i _ ->
      mk_named_assert (sprintf "VC_%d" i)
        (mk_atom (sprintf "P_%d" i) *. mk_atom "a2"))
  @ postlude true false
  |> _U_to_string

let gen_P'_select_one u a2 =
  let _P' = mk_atom "P_" in
  prelude false false @ !m.udaf
  @ [gen_P'_one _P' u; a2; mk_assert (_P' *. mk_atom "a2")]
  @ postlude false false
  |> _U_to_string

let gen_VC2_one_cex _U_Q _U_BI u =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI = mk_atom "BI" in
  let _BI_0 = mk_atom "BI_0" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_all _Q _U_Q;
      gen_BI_all _BI _U_BI;
      gen_BI_one _BI_0 u;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_declare_const r !m._Row;
      mk_assert
        ~.(mk_list [_BI; a1; a2]
          &&. _Q *. r
          =>. mk_list [_BI_0; mk_list [!m.f; a1; r]; mk_list [!m.f; a2; r]]);
    ]
  @ postlude false true
  |> _U_to_string

let gen_VC3_one_cex _U_Q _U_BI u =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI = mk_atom "BI" in
  let _BI_0 = mk_atom "BI_0" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_all _Q _U_Q;
      gen_BI_all _BI _U_BI;
      gen_BI_one _BI_0 u;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_declare_const r !m._Row;
      mk_assert
        ~.(mk_list [_BI; a1; a2]
          &&. ~.(_Q *. r)
          =>. mk_list [_BI_0; mk_list [!m.f; a1; r]; a2]);
    ]
  @ postlude false true
  |> _U_to_string

let gen_VC2_cex _U_Q _U_BI_max =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI_min = mk_atom "BI_min" in
  let _BI_max = mk_atom "BI_max" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_all _Q _U_Q;
      gen_BI_one _BI_min !m._BI_min;
      gen_BI_all _BI_max _U_BI_max;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_declare_const r !m._Row;
      mk_assert
        ~.(mk_list [_BI_max; a1; a2]
          &&. _Q *. r
          =>. mk_list [_BI_min; mk_list [!m.f; a1; r]; mk_list [!m.f; a2; r]]);
    ]
  @ postlude false true
  |> _U_to_string

let gen_VC3_cex _U_Q _U_BI_max =
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let r = mk_atom "r" in
  let _Q = mk_atom "Q" in
  let _BI_min = mk_atom "BI_min" in
  let _BI_max = mk_atom "BI_max" in
  prelude false false @ !m.udaf
  @ [
      gen_Q_all _Q _U_Q;
      gen_BI_one _BI_min !m._BI_min;
      gen_BI_all _BI_max _U_BI_max;
      mk_declare_const a1 !m._State;
      mk_declare_const a2 !m._State;
      mk_declare_const r !m._Row;
      mk_assert
        ~.(mk_list [_BI_max; a1; a2]
          &&. ~.(_Q *. r)
          =>. mk_list [_BI_min; mk_list [!m.f; a1; r]; a2]);
    ]
  @ postlude false true
  |> _U_to_string

let check_BI_bounds _U_BI_max = not (solve (gen_BI_comp _U_BI_max)).sat

let find_lower_init _U_BI =
  List.filter _U_BI ~f:(fun u ->
      let encoding = gen_VC4_one u in
      let sol = solve encoding in
      not sol.sat)

let rec find_lower_fix _U_BI _BI =
  let encoding = gen_VC4_P_cex true _BI in
  let sol = solve encoding in
  if not sol.sat then
    (* save_encoding !m.save "test_find_lower_fix" encoding; *)
    _BI
  else
    let a1_def =
      List.find_map_exn sol.model ~f:(fun (k, v) ->
          Option.some_if String.(k = "a1") v)
    in
    let a2_def =
      List.find_map_exn sol.model ~f:(fun (k, v) ->
          Option.some_if String.(k = "a2") v)
    in
    (* printf "a1: %s\n" (Sexp.to_string_hum a1_def);
    printf "a2: %s\n" (Sexp.to_string_hum a2_def); *)
    let encoding = gen_BI_select _U_BI a1_def a2_def in
    (* save_encoding true "test_BI_select" encoding; *)
    let sol = solve encoding in
    if sol.sat then raise Unreachable
    else
      find_lower_fix
        (match List.filteri _U_BI ~f:(fun i _ -> i <> sol.unsat_core) with
        | [] ->
          (* raise Unreachable; *)
          [sexp_true]
        | _U_BI' -> _U_BI')
        (List.nth_exn _U_BI sol.unsat_core :: _BI)

let refine_lower _U_BI =
  if !m.d >= 0 then printf "[%d] === find_lower ===\n%!" !m.iters;
  let _U_BI' =
    match find_lower_init _U_BI with
    | [] -> [sexp_true]
    | _U_BI_init -> _U_BI_init
  in
  find_lower_fix _U_BI' [sexp_true]

let refine_upper_init _U_BI =
  let m' = !m in
  m := {!m with solver = "z3"; solver_args = z3_args};
  let _U_BI' =
    List.filter _U_BI ~f:(fun u ->
        let encoding = gen_VC1_one u in
        let sol = solve encoding in
        sol.sat)
  in
  m := m';
  _U_BI'

let refine_upper _U_BI _U_Q =
  List.fold_until _U_BI ~init:(Ok [])
    ~f:(fun acc u ->
      let encoding = gen_VC2_one_cex _U_Q _U_BI u in
      let sol = solve encoding in
      if sol.sat then
        if Sexp.(u = !m._BI_min) then
          (* if is_abl_noanalysis () then
            Continue (Result.map acc ~f:(List.cons u))
          else *)
          let r_def =
            List.find_map_exn sol.model ~f:(fun (k, v) ->
                Option.some_if String.(k = "r") v)
          in
          Stop (Error (true, r_def))
        else Continue acc
      else Continue (Result.map acc ~f:(List.cons u)))
    ~finish:Fn.id

(* let rec repair_Q _U_Q diagnosis =
  if !m.d >= 0 then printf "[%d] === repair ===\n%!" !m.iters;
  let encoding = gen_Q_select _U_Q diagnosis in
  let sol = solve encoding in
  if sol.sat then _U_Q
  else
    repair_Q (List.filteri _U_Q ~f:(fun i _ -> i <> sol.unsat_core)) diagnosis *)

(* The Repair helper in Algorithm 1: given a Sync(r) or Stutter(r) diagnosis,
   return the repaired candidate Q. *)
let repair_Q _U_Q diagnosis =
  _U_Q
  |> List.foldi ~init:([], -1) ~f:(fun i (repaired, idx) u ->
      let encoding = gen_Q_select_one u diagnosis in
      let sol = solve encoding in
      if sol.sat then (u :: repaired, idx) else (repaired, i))
  |> Tuple2.map_fst ~f:List.rev

(* CheckUnrealizable (Algorithm 4): given the current symbolic upper bound
   psi_max, decide whether no invariant in that range can witness Q being a
   sound pushdown. *)
let check_unrealizable _U_Q _U_BI_max =
  let open Result.Let_syntax in
  let encoding = gen_VC2_cex _U_Q _U_BI_max in
  let sol = solve encoding in
  let%bind () =
    if sol.sat then
      let r_def =
        List.find_map_exn sol.model ~f:(fun (k, v) ->
            Option.some_if String.(k = "r") v)
      in
      Error (true, r_def)
    else Ok ()
  in
  let encoding = gen_VC3_cex _U_Q _U_BI_max in
  let sol = solve encoding in
  if sol.sat then
    let r_def =
      List.find_map_exn sol.model ~f:(fun (k, v) ->
          Option.some_if String.(k = "r") v)
    in
    Error (false, r_def)
  else Ok ()

(* WeakenViaBounds (Algorithm 2). Alternates tightening the symbolic bounds
   (RefineBounds, Algorithm 3) with unrealizability checking until either Q
   survives or Q is repaired down to the empty filter. *)
let rec weaken_via_bounds _Q _U_BI_max =
  if !m.d >= 0 then printf "[%d] === weaken_via_bounds ===\n%!" !m.iters;
  if List.is_empty _Q then None
  else
    match refine_upper _U_BI_max _Q with
    | Error diagnosis ->
      if is_abl_noanalysis () then None
      else weaken_via_bounds (fst (repair_Q _Q diagnosis)) _U_BI_max
    | Ok _U_BI_max' -> (
      match check_unrealizable _Q _U_BI_max' with
      | Ok () -> Some (_Q, _U_BI_max')
      | Error diagnosis ->
        if is_abl_noanalysis () then None
        else weaken_via_bounds (fst (repair_Q _Q diagnosis)) _U_BI_max')

let weaken_via_vc _U_Q _BI_cand ~fwd =
  let gen_VC = if fwd then gen_VC2_one_cex else gen_VC3_one_cex in
  List.fold_until _BI_cand ~init:(Ok [])
    ~f:(fun acc u ->
      let encoding = gen_VC _U_Q _BI_cand u in
      let sol = solve encoding in
      if sol.sat then
        if (not (is_abl_nobounds ())) && Sexp.(u = !m._BI_min) then
          let r_def =
            List.find_map_exn sol.model ~f:(fun (k, v) ->
                Option.some_if String.(k = "r") v)
          in
          Stop (Error (fwd, r_def))
        else Continue acc
      else Continue (Result.map acc ~f:(List.cons u)))
    ~finish:Fn.id

(* FindStrongestBisimulation (Algorithm 5): tighten the symbolic upper bound
   U_BI_max against the Sync and Stutter VCs until it stops shrinking, or
   return a diagnosis pointing at the VC that rules Q out. *)
let rec find_strongest_bisim _U_Q _U_BI_max =
  (* if !m.d >= 0 then printf "[%d] === find_strongest_bisim ===\n%!" !m.iters; *)
  let open Result.Let_syntax in
  let%bind _U_BI_max' = weaken_via_vc _U_Q _U_BI_max ~fwd:true in
  let%bind _U_BI_max'' = weaken_via_vc _U_Q _U_BI_max' ~fwd:false in
  if List.length _U_BI_max'' = List.length _U_BI_max then Ok _U_BI_max''
  else find_strongest_bisim _U_Q _U_BI_max''

let rec select_P' selected _U_P' a2 =
  if List.is_empty _U_P' then selected
  else
    let encoding = gen_P'_select _U_P' a2 in
    let sol = solve encoding in
    if sol.sat then selected
    else
      let rel, irrel =
        List.foldi _U_P' ~init:([], []) ~f:(fun i (rels, irrels) u ->
            if i = sol.unsat_core then (u :: rels, irrels)
            else (rels, u :: irrels))
      in
      select_P' (selected @ rel) irrel a2

(* let select_P' _U_P' a2 =
  List.filter _U_P' ~f:(fun u ->
      let encoding = gen_P'_select_one u a2 in
      let sol = solve encoding in
      not sol.sat) *)

let enqueue_P's heap _P' _U_P'1 _U_P'2 =
  List.fold _U_P'1 ~init:heap ~f:(fun acc u ->
      P'Heap.insert acc (u :: _P', List.filter _U_P'2 ~f:(Sexp.( <> ) u)))

let rec find_residual_aux heap seen _U_BI _Q =
  (* if !m.d >= 0 then printf "[%d] === find_residual_aux ===\n%!" !m.iters; *)
  let _P', _U_P' = P'Heap.find_min heap in
  let heap' = P'Heap.del_min heap in
  let _P'_set = Set.of_list (module Sexp) _P' in
  if Set.mem seen _P'_set then find_residual_aux heap' seen _U_BI _Q
  else
    let seen' = Set.add seen _P'_set in
    Hash_set.add pairs (_Q, _P'_set);
    let encoding = gen_VC4_distinguish _U_BI _P' in
    let sol = solve encoding in
    if sol.sat then
      (* save_encoding true "test_find_residual_aux" encoding; *)
      Some _P'
    else if
      List.is_empty _U_P' || ((not (is_abl_noanalysis ())) && sol.unsat_core = 1)
    then find_residual_aux heap' seen' _U_BI _Q
    else if is_abl_noanalysis () then
      find_residual_aux (enqueue_P's heap' _P' _U_P' _U_P') seen' _U_BI _Q
    else
      let encoding = gen_VC4_sub_cex _U_BI _P' in
      let sol = solve encoding in
      if not sol.sat then raise Unreachable;
      let a2_def =
        List.find_map_exn sol.model ~f:(fun (k, v) ->
            Option.some_if String.(k = "a2") v)
      in
      (* printf "a2: %s\n" (Sexp.to_string_hum a2_def); *)
      let rels = select_P' [] _U_P' a2_def in
      (* printf "|rels| = %d\n%!" (List.length rels); *)
      find_residual_aux (enqueue_P's heap' _P' rels _U_P') seen' _U_BI _Q

(* FindResidual (Algorithm 6): given a sound (Q, psi), search for the weakest
   P' in U_P' such that (Q, P', psi) satisfies the Final VC. *)
let find_residual _U_BI _U_P' _Q =
  if !m.d >= 0 then printf "[%d] === find_residual ===\n%!" !m.iters;
  let _P =
    !m.decls
    |> List.find_map_exn ~f:(fun (Ast.Ident.Ident s, (_P, _)) ->
        Option.some_if (String.is_suffix s ~suffix:"_P") _P)
    |> get_fun_body
  in
  Hash_set.add pairs (_Q, !m._P_comps);
  let encoding = gen_VC4_P_cex false _U_BI in
  (* save_encoding !m.save "test_find_residual" encoding; *)
  if (solve encoding).sat then None
  else
    find_residual_aux
      (P'Heap.of_list [([sexp_true], _U_P')])
      (Set.empty (module SexpSet))
      _U_BI _Q

let rec find_residual_bisim heap seen _BI _Q =
  if !m.d >= 0 then printf "[%d] === find_residual_bisim ===\n%!" !m.iters;
  if P'Heap.size heap = 0 then None
  else
    let _P', _U_P' = P'Heap.find_min heap in
    let heap' = P'Heap.del_min heap in
    let _P'_set = Set.of_list (module Sexp) _P' in
    if Set.mem seen _P'_set then find_residual_bisim heap' seen _BI _Q
    else
      let seen' = Set.add seen _P'_set in
      Hash_set.add pairs (Set.of_list (module Sexp) _Q, _P'_set);
      match refine_upper _BI _Q with
      | Ok _BI' -> (
        match find_strongest_bisim _Q _BI' with
        | Ok _BI'' ->
          let encoding = gen_VC4_distinguish _BI'' _P' in
          let sol = solve encoding in
          if sol.sat then
            (* save_encoding true "test_find_residual_twophase" encoding; *)
            Some (_P', _BI'')
          else if List.is_empty _U_P' || sol.unsat_core = 1 then
            find_residual_bisim heap' seen' _BI _Q
          else
            let encoding = gen_VC4_sub_cex _BI'' _P' in
            let sol = solve encoding in
            if not sol.sat then raise Unreachable;
            let a2_def =
              List.find_map_exn sol.model ~f:(fun (k, v) ->
                  Option.some_if String.(k = "a2") v)
            in
            (* printf "a2: %s\n" (Sexp.to_string_hum a2_def); *)
            let rels = select_P' [] _U_P' a2_def in
            (* printf "|rels| = %d\n%!" (List.length rels); *)
            find_residual_bisim
              (enqueue_P's heap' _P' rels _U_P')
              seen' _BI'' _Q
        | Error _ ->
          find_residual_bisim (enqueue_P's heap' _P' _U_P' _U_P') seen' _BI' _Q)
      | Error _ ->
        find_residual_bisim (enqueue_P's heap' _P' _U_P' _U_P') seen' _BI _Q

(* let infer () =
  let encoding = gen_infer_BI_P' [] in
  save_encoding true "test_infer" encoding;
  let solver_id, solver_args =
    if is_abl_spacer () then ("z3", z3_args)
    else if is_abl_eldarica () then ("eld", eld_args ())
    else raise Unreachable
  in
  let sol = solve solver_id solver_args encoding in
  if sol.sat then
    let _Q =
      sol.model
      |> List.find_map_exn ~f:(fun (k, v) -> Option.some_if String.(k = "Q") v)
      |> get_fun_body
    in
    let _BI =
      sol.model
      |> List.find_map_exn ~f:(fun (k, v) -> Option.some_if String.(k = "BI") v)
      |> get_fun_body
    in
    let _P' =
      sol.model
      |> List.find_map_exn ~f:(fun (k, v) -> Option.some_if String.(k = "P_") v)
      |> get_fun_body
    in
    Some (_Q, _BI, _P')
  else None *)

let enqueue_Qs queue _U_Q =
  if List.length _U_Q = 1 then queue
  else
    List.foldi _U_Q ~init:queue ~f:(fun i acc _ ->
        Fke.push acc (List.filteri _U_Q ~f:(fun j _ -> j <> i)))

(* SynthesizeOptimalPushdown (Algorithm 1), main loop. Walks candidate filters
   Q in decreasing order of strength, repairing or enqueuing weakenings until
   a sound (Q, P') is returned. *)
let rec synth_opt_pushdown queue _U_BI _U_P' =
  m := {!m with iters = !m.iters + 1};
  if !m.d >= 0 then printf "[%d] === synth_opt_pushdown ===\n%!" !m.iters;
  match Fke.pop queue with
  | None -> synth_opt_pushdown (Fke.push queue [sexp_true]) _U_BI _U_P'
  | Some (_U_Q, queue') -> (
    if is_abl_twophase () then
      match
        find_residual_bisim
          (P'Heap.of_list [([sexp_true], _U_P')])
          (Set.empty (module SexpSet))
          _U_BI _U_Q
      with
      | Some (_P', _BI) -> Some (mk_and _U_Q, mk_and _BI, mk_and _P')
      | None -> synth_opt_pushdown (enqueue_Qs queue' _U_Q) _U_BI _U_P'
    else if is_abl_spacer () || is_abl_eldarica () then (
      let encoding = gen_infer_BI_P' _U_Q in
      save_encoding !m.save "test_chc_mode" encoding;
      let sol = solve encoding in
      (* print_solution sol; *)
      if sol.sat then
        let _BI =
          sol.model
          |> List.find_map_exn ~f:(fun (k, v) ->
              Option.some_if String.(k = "BI") v)
          |> get_fun_body
        in
        let _P' =
          sol.model
          |> List.find_map_exn ~f:(fun (k, v) ->
              Option.some_if String.(k = "P_") v)
          |> get_fun_body
        in
        Some (mk_and _U_Q, _BI, _P')
        (* else if
        match _U_Q with [u] when Sexp.(u = sexp_true) -> true | _ -> false
      then None *)
      else synth_opt_pushdown (enqueue_Qs queue' _U_Q) _U_BI _U_P')
    else
      let wvb_result =
        if is_abl_nobounds () then Some (_U_Q, _U_BI)
        else weaken_via_bounds _U_Q _U_BI
      in
      match wvb_result with
      | None ->
        if is_abl_noanalysis () then
          synth_opt_pushdown (enqueue_Qs queue' _U_Q) _U_BI _U_P'
        else synth_opt_pushdown queue' _U_BI _U_P'
      | Some (_U_Q', _U_BI_max) -> (
        if !m.d >= 0 then
          printf "[%d] === find_strongest_bisim ===\n%!" !m.iters;
        let _U_BI_strongest = find_strongest_bisim _U_Q' _U_BI_max in
        match _U_BI_strongest with
        | Error diagnosis ->
          if is_abl_noanalysis () then
            synth_opt_pushdown (enqueue_Qs queue' _U_Q') _U_BI_max _U_P'
            (* synth_opt_pushdown (enqueue_Qs queue' _U_Q') _U_BI _U_P' *)
          else
            let repaired, idx = repair_Q _U_Q' diagnosis in
            let queue'' =
              if List.length _U_Q' = 1 then Fke.push queue' repaired
              else
                let prioritize = List.length repaired = List.length _U_Q' - 1 in
                let queue'' =
                  List.foldi _U_Q'
                    ~init:
                      (if prioritize then Fke.push queue' repaired else queue')
                    ~f:(fun i acc _ ->
                      if prioritize && i = idx then acc
                      else
                        Fke.push acc (List.filteri _U_Q' ~f:(fun j _ -> j <> i)))
                in
                if prioritize then queue'' else Fke.push queue'' repaired
            in
            synth_opt_pushdown queue'' _U_BI_max _U_P'
            (* synth_opt_pushdown queue'' _U_BI _U_P' *)
        | Ok _U_BI_strongest -> (
          let fr_result =
            find_residual _U_BI_strongest _U_P'
              (Set.of_list (module Sexp) _U_Q')
          in
          match fr_result with
          | Some _U_P' ->
            Some (mk_and _U_Q', mk_and _U_BI_strongest, mk_and _U_P')
          | None -> synth_opt_pushdown (enqueue_Qs queue' _U_Q') _U_BI_max _U_P'
          (* synth_opt_pushdown (enqueue_Qs queue' _U_Q') _U_BI _U_P' *))))

let get_kind _Q _BI _P' =
  m := {!m with solver = "z3"; solver_args = z3_args};
  let encoding = gen_all _Q _BI _P' in
  save_encoding !m.save "verify_MinPooling_4_1" encoding;
  if try (solve encoding).sat with Solver_unknown -> true then -1
  else if not (solve (gen_Q_comp ~op:( =. ) _Q sexp_true)).sat then 0
  else if
    (not (solve (gen_P'_comp_kind ~op:( =. ) false sexp_true _P')).sat)
    || not
         (solve
            (gen_P'_comp_kind ~op:( =. ) false ~.(mk_atom "a" =. !m._I) _P'))
           .sat
  then 1
  else
    let _P =
      !m.decls
      |> List.find_map_exn ~f:(fun (Ident id, (_P_def, _)) ->
          Option.some_if String.(id = get_atom_str !m._P) _P_def)
      |> get_fun_body
    in
    let forward = gen_P'_comp_kind false _P _P' in
    let backward = gen_P'_comp_kind true _P' _P in
    save_encoding !m.save "test_forward" forward;
    save_encoding !m.save "test_backward" backward;
    match
      ( not (try (solve forward).sat with Solver_unknown -> true),
        not (try (solve backward).sat with Solver_unknown -> true) )
    with
    | true, true -> 2
    | true, false -> 3
    | false, true -> 4
    | false, false -> 5

let magicpush_possible _Q =
  let encoding_exact = gen_magicpush_preconds_Q _Q in
  (* save_encoding true "test_magicpush_exact" encoding_exact; *)
  let encoding_partial = gen_magicpush_preconds_Q ~partial:true _Q in
  (* save_encoding true "test_magicpush_partial" encoding_partial; *)
  m := {!m with solver = "cvc5"; solver_args = cvc5_args};
  try (not (solve encoding_exact).sat, not (solve encoding_partial).sat)
  with Solver_unknown -> (false, false)

(* Top-level entry called from pusharoo.ml. Parses, runs the analysis passes,
   builds U_Q / U_P' / U_psi, and invokes synth_opt_pushdown. *)
let synth raw =
  let ast =
    raw |> Lexing.from_string |> Parser.start Lexer.token |> Pass_reduce.reduce
    |> Pass_gc.gc |> List.hd_exn
  in
  if !m.d >= 1 then printf "AST:\n%s\n\n" (Ast.show_stmt ast);
  (* let ast_size = Ast.size_of_stmt ast in
  printf "%d\n" ast_size;
  exit 0; *)
  let decls, order = Pass_encode.encode ast in
  if !m.d >= 2 then (
    decls |> Map.to_alist
    |> List.map ~f:(fun (id, (decl, decl_type)) ->
        sprintf "%s: %s : %s" (Ast.Ident.show id) (Sexp.to_string_hum decl)
          (Sexp.to_string_hum decl_type))
    |> String.concat ~sep:"\n"
    |> printf "Typed decls:\n%s\n\n";
    order
    |> List.rev_map ~f:Ast.Ident.show
    |> String.concat ~sep:", " |> printf "Order:\n%s\n\n");
  let udaf = Pass_encode.to_sorted_list decls order in
  let decls = Map.to_alist decls in
  let _State_def, _State, _Row_def, _Row, f, _I, _P =
    Pass_encode.extract_defs decls
  in
  let ((cfg, _rev_cfg) as cfgs) = Pass_cfa.analyze ast in
  (* Pass_cfa.print_cfg "CFG" cfg Pass_cfa.Labeled_ident.show;
  Pass_cfa.print_cfg "Rev CFG" _rev_cfg Pass_cfa.Indexed_ident.show; *)
  (* let has_indep =
    Map.length cfg > 1
    && cfg |> Map.to_alist
       |> List.exists ~f:(function (a, i), targets ->
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
  printf "%b,%b,%b\n" (Map.length cfg > 1) has_indep has_crossdep;
  exit 0; *)
  let monos = Pass_monos.infer_monos ast cfg in
  (* printf "Mono:\n%s\n"
    (monos
    |> List.mapi ~f:(fun i mono ->
           sprintf "- %d: %s" i (Pass_monos.show_mono mono))
    |> String.concat ~sep:"\n"); *)
  let a1 = mk_atom "a1" in
  let a2 = mk_atom "a2" in
  let _BI_min =
    mk_and [_P *. a1; _P *. a2; a1 =. a2] ||. (~.(_P *. a1) &&. ~.(_P *. a2))
  in
  m :=
    {
      !m with
      decls;
      udaf;
      _State_def;
      _State;
      _Row_def;
      _Row;
      f;
      _I;
      _P;
      _BI_min;
    };
  let init_U_Q, init_U_BI, init_U_P', _P_comps =
    Extract.extract_Us !m ast cfgs monos
  in
  (* printf "|U_Q| = %d\n" (List.length init_U_Q);
  printf "|U_BI| = %d\n" (List.length init_U_BI);
  printf "|U_P'| = %d\n" (List.length init_U_P'); *)
  m := {!m with _P_comps};
  let init_U_BI = _BI_min :: init_U_BI in
  (* let _U_BI_min = refine_lower init_U_BI in
  m := {!m with _BI_min = _U_BI_min}; *)
  let _U_BI_max = refine_upper_init init_U_BI in
  (* if not (check_BI_bounds _U_BI_max) then failwith "BI_min > BI_max"; *)
  let sol =
    (* if !m.infer_all then infer () else *)
    synth_opt_pushdown (Fke.push Fke.empty init_U_Q) _U_BI_max init_U_P'
  in
  (sol, _P_comps, Hash_set.length pairs)
