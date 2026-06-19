open Core
open Re
open Ast
open Exns

let dq_char = Char.of_int_exn 34 (* double-quote character *)
let mk_atom s = Sexp.Atom s
let is_atom t = match t with Sexp.Atom _ -> true | _ -> false
let mk_list l = Sexp.List l
let mk_unary a = mk_list [a]
let mk_pair a1 a2 = mk_list [a1; a2]
let ( *. ) = mk_pair

let list_length = function
  | Sexp.List (Sexp.Atom "and" :: l) -> List.length l
  | _ -> 1

let list_split = function Sexp.List (Sexp.Atom "and" :: l) -> l | s -> [s]

let mk_list_norm hd = function
  | Sexp.List (Sexp.List _ :: _ as inner) -> mk_list (hd :: inner)
  | sexp -> hd *. sexp

let ( **. ) = mk_list_norm

let get_atom_str = function
  | Sexp.Atom s -> s
  | _ -> raise (Invalid_argument "Expected Atom")

let sexp_set_option = mk_atom "set-option"
let sexp_unused = mk_atom "_"
let sexp_null = mk_atom "None"
let sexp_declare_datatype = mk_atom "declare-datatype"
let sexp_declare_fun = mk_atom "declare-fun"
let sexp_define_fun = mk_atom "define-fun"
let sexp_define_fun_rec = mk_atom "define-fun-rec"
let sexp_declare_const = mk_atom "declare-const"
let sexp_define_const = mk_atom "define-const"
let sexp_int = mk_atom "Int"

let mk_int i =
  if Utils.is_abl_eldarica () && i < 0 then
    mk_list [mk_atom "-"; mk_atom (Int.to_string (Int.abs i))]
  else mk_atom (Int.to_string i)

let sexp_real = mk_atom "Real"

let mk_real f =
  let s = Float.to_string f in
  let s = if String.is_suffix s ~suffix:"." then s ^ "0" else s in
  mk_atom s

let sexp_string = mk_atom "String"
let mk_string s = mk_atom (Format.sprintf "\"%s\"" s)
let sexp_strlen = mk_atom "str.len"
let sexp_list = mk_pair (mk_atom "List")
let sexp_nil = mk_atom "nil"
let sexp_insert = mk_atom "insert"
let mk_insert hd tl = mk_list [sexp_insert; hd; tl]
let ( ++. ) = mk_insert
let sexp_head = mk_atom "head"
let mk_head l = sexp_head *. l
let sexp_tail = mk_atom "tail"
let mk_tail l = sexp_tail *. l
let mk_List = List.fold ~init:sexp_nil ~f:(fun acc sexp -> sexp ++. acc)

let is_List t =
  match t with Sexp.List [Sexp.Atom "List"; _] -> true | _ -> false

let get_List_type l =
  match l with
  | Sexp.List [Sexp.Atom "List"; t] -> t
  | _ -> raise (Invalid_argument "Expected List")

let sexp_add = mk_atom "+"
let sexp_sub = mk_atom "-"
let sexp_mul = mk_atom "*"
let sexp_div = mk_atom "/"
let mk_add lhs rhs = mk_list [sexp_add; lhs; rhs]
let mk_sub lhs rhs = mk_list [sexp_sub; lhs; rhs]
let mk_mul lhs rhs = mk_list [sexp_mul; lhs; rhs]
let mk_div lhs rhs = mk_list [sexp_div; lhs; rhs]
let sexp_sub = mk_atom "-"
let sexp_bool = mk_atom "Bool"
let sexp_true = mk_atom "true"
let sexp_false = mk_atom "false"
let mk_bool b = if b then sexp_true else sexp_false
let sexp_eq = mk_atom "="
let sexp_ge = mk_atom ">="
let sexp_gt = mk_atom ">"
let sexp_le = mk_atom "<="
let sexp_lt = mk_atom "<"
let sexp_and = mk_atom "and"
let sexp_or = mk_atom "or"
let sexp_not = mk_atom "not"
let sexp_ite = mk_atom "ite"
let mk_eq lhs rhs = mk_list [sexp_eq; lhs; rhs]
let ( =. ) = mk_eq
let mk_ge lhs rhs = mk_list [sexp_ge; lhs; rhs]
let ( >=. ) = mk_ge
let mk_gt lhs rhs = mk_list [sexp_gt; lhs; rhs]
let ( >. ) = mk_gt
let mk_le lhs rhs = mk_list [sexp_le; lhs; rhs]
let ( <=. ) = mk_le
let mk_lt lhs rhs = mk_list [sexp_lt; lhs; rhs]
let ( <. ) = mk_lt
let mk_bin_and lhs rhs = mk_list [sexp_and; lhs; rhs]
let ( &&. ) = mk_bin_and
let mk_and conjuncts = mk_list (sexp_and :: conjuncts)
let mk_bin_or lhs rhs = mk_list [sexp_or; lhs; rhs]
let ( ||. ) = mk_bin_or
let mk_or disjuncts = mk_list (sexp_or :: disjuncts)
let mk_not = mk_pair sexp_not
let ( ~. ) = mk_not
let mk_ite cond then_ else_ = mk_list [sexp_ite; cond; then_; else_]
let sexp_assert = mk_atom "assert"
let sexp_bang = mk_atom "!"
let sexp_forall = mk_atom "forall"
let sexp_exists = mk_atom "exists"
let sexp_implies = mk_atom "=>"
let sexp_named = mk_atom ":named"
let mk_implies ante conseq = mk_list [sexp_implies; ante; conseq]
let ( =>. ) = mk_implies
let mk_forall params body = mk_list [sexp_forall; mk_list params; body]
let ( |. ) = mk_forall
let mk_exists params body = mk_list [sexp_exists; mk_list params; body]
let ( =|. ) = mk_exists
let mk_assert body = mk_list [sexp_assert; body]

let mk_named_assert name body =
  mk_list [sexp_assert; mk_list [sexp_bang; body; sexp_named; mk_atom name]]

let mk_datatype name mk_constrs mk_accrs =
  mk_list
    [
      sexp_declare_datatype;
      mk_atom name;
      mk_list
        (mk_constrs (fun i ->
             mk_list
               (mk_atom (Format.sprintf "%s%d" name i)
               :: List.filter_opt
                    (mk_accrs (fun i' j t ->
                         if i = i' then
                           Some
                             (mk_atom (Format.sprintf "%s%d!%d" name i j) *. t)
                         else None)))));
    ]

let mk_declare_fun name domain range =
  mk_list [sexp_declare_fun; mk_atom name; mk_list domain; range]

let mk_define_fun is_rec name params range body =
  mk_list
    [
      (if is_rec then sexp_define_fun_rec else sexp_define_fun);
      name;
      mk_list params;
      range;
      body;
    ]

let get_fun_body = function
  | Sexp.List [Sexp.Atom "define-fun"; _; Sexp.List _; Sexp.Atom _; body] ->
    body
  | _ -> raise (Invalid_argument "Invalid function format")

let mk_declare_const name t = mk_list [sexp_declare_const; name; t]

let mk_define_const name t body =
  mk_list [sexp_define_const; mk_atom name; mk_atom t; body]

let get_const_body = function
  | Sexp.List [Sexp.Atom "define-fun"; _; Sexp.List []; Sexp.Atom _; body] ->
    body
  | _ -> raise (Invalid_argument "Invalid constant format")

let get_const_fields const =
  match const with
  | Sexp.List [Sexp.Atom "define-const"; _; _; Sexp.List (_ :: fields)] ->
    fields
  | _ -> raise (Invalid_argument "Invalid constant format")

let has_name = function Sexp.List (_ :: Sexp.Atom _ :: _) -> true | _ -> false

let get_name = function
  | Sexp.List (_ :: (Sexp.Atom _ as name) :: _) -> name
  | _ -> raise (Invalid_argument "Invalid sexp format")

let get_constr datatype i =
  match datatype with
  | Sexp.List [_; _; Sexp.List constructors] -> (
    match List.nth_exn constructors i with
    | Sexp.List ((Sexp.Atom _ as constructor) :: _) -> constructor
    | _ -> raise (Invalid_argument "Invalid constructor format"))
  | _ -> raise (Invalid_argument "Invalid datatype format")

let get_typed_accr datatype i j =
  match datatype with
  | Sexp.List [_; _; Sexp.List constructors] -> (
    match List.nth_exn constructors i with
    | Sexp.List (_ :: accessors) -> (
      match List.nth_exn accessors j with
      | Sexp.List [(Sexp.Atom _ as accessor); accessor_type] ->
        (accessor, accessor_type)
      | _ -> raise (Invalid_argument "Invalid accessor format"))
    | _ -> raise (Invalid_argument "Invalid constructor format"))
  | _ -> raise (Invalid_argument "Invalid datatype format")

let get_typed_accrs datatype i =
  match datatype with
  | Sexp.List [_; _; Sexp.List constructors] -> (
    match List.nth_exn constructors i with
    | Sexp.List (_ :: accessors) ->
      List.map accessors ~f:(function
        | Sexp.List [(Sexp.Atom _ as accessor); accessor_type] ->
          (accessor, accessor_type)
        | _ -> raise (Invalid_argument "Invalid accessor format"))
    | _ -> raise (Invalid_argument "Invalid constructor format"))
  | _ -> raise (Invalid_argument "Invalid datatype format")

let get_recg datatype i =
  match datatype with
  | Sexp.List [_; _; Sexp.List constructors] -> (
    match List.nth_exn constructors i with
    | Sexp.List ((Sexp.Atom _ as constructor) :: _) ->
      mk_atom (Format.asprintf "is-%a" Sexp.pp constructor)
    | _ -> raise (Invalid_argument "Invalid constructor format"))
  | _ -> raise (Invalid_argument "Invalid datatype format")

let get_range = function
  | Sexp.List [_; _; _; (Sexp.Atom _ as range); _] -> range
  | _ -> raise (Invalid_argument "Invalid function format")

let mk_option name t =
  mk_datatype name
    (fun mk_constr -> [mk_constr 0; mk_constr 1])
    (fun mk_accr -> [mk_accr 1 0 t])

let opt_suffix = "_opt"

let type_ref_to_opt type_ref =
  Format.asprintf "%a%s" pp_type_ref type_ref opt_suffix

let is_opt t =
  is_atom t && Stdlib.String.ends_with (get_atom_str t) ~suffix:opt_suffix

let rec sexp_to_smtlib buf = function
  | Sexp.Atom s -> Buffer.add_string buf s
  | Sexp.List l ->
    Buffer.add_char buf '(';
    List.iteri l ~f:(fun i sub ->
        if i > 0 then Buffer.add_char buf ' ';
        sexp_to_smtlib buf sub);
    Buffer.add_char buf ')'

let sexp_to_smtlib_string sexp =
  let buf = Buffer.create 256 in
  sexp_to_smtlib buf sexp;
  Buffer.contents buf

let get_inner_type opt =
  match get_atom_str opt with
  | "Int_opt" -> sexp_int
  | "Real_opt" -> sexp_real
  | "Bool_opt" -> sexp_bool
  | "String_opt" -> sexp_string
  | s when execp (Perl.compile_pat {|^List_.+_opt$|}) s ->
    sexp_list
      (mk_atom (Group.get (Re.exec (Perl.compile_pat {|^List_(.+)_opt$|}) s) 1))
  | _ -> raise Unreachable
