(* --------------------------------------------------------------- *)
(* ------------------------Initialisation ------------------------ *)

type tformula =
| Value of bool (* ⊥ ou � *)
| Var of string (* Variable *)
| Not of tformula (* Negation *)
| And of tformula * tformula (* Conjonction *)
| Or of tformula * tformula (* Disjonction *)
| Implies of tformula * tformula (* Implication *)
| Equivalent of tformula * tformula (* Equivalence *)
;;

type decTree =
| DecLeaf of bool
| DecRoot of string * decTree * decTree;;

let p1 = Var "P1";;
let p2 = Var "P2";;
let q1 = Var "Q1";;
let q2 = Var "Q2";;
let f1 = Equivalent (q1, q2);;
let f2 = Equivalent (p1, p2);;
let ex1 = And (f1, f2);;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 1 -------------------------- *)

(* val getVars : tformula -> string list *)
let getVars formula =
  let rec aux vars formula =
    match formula with
    | Value _ -> vars
    | Var v -> v :: vars
    | Not f -> aux vars f
    | And (f1, f2)
    | Or (f1, f2)
    | Implies (f1, f2)
    | Equivalent (f1, f2) ->
        let vars' = aux vars f1 in
        aux vars' f2
  in
  List.sort_uniq compare (aux [] formula);;

(* --- test --- *)
(* val string_of_var : tformula -> string *)
let rec string_of_var = function
  | Var v -> v
  | Value _ -> failwith "Invalid argument: Value"
  | Not _ -> failwith "Invalid argument: Not"
  | And _ -> failwith "Invalid argument: And"
  | Or _ -> failwith "Invalid argument: Or"
  | Implies _ -> failwith "Invalid argument: Implies"
  | Equivalent _ -> failwith "Invalid argument: Equivalent";;

assert (getVars ex1 = [string_of_var p1; string_of_var p2; string_of_var q1; string_of_var q2]);;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)
