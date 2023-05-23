(* --------------------------------------------------------------- *)
(* ------------------------Initialisation ------------------------ *)

type tformula =
  | Value of bool (* ⊥ ou ⊤ *)
  | Var of string (* Variable *)
  | Not of tformula (* Negation *)
  | And of tformula * tformula (* Conjonction *)
  | Or of tformula * tformula (* Disjonction *)
  | Implies of tformula * tformula (* Implication *)
  | Equivalent of tformula * tformula (* Equivalence *)
;;

type decTree =
  | DecLeaf of bool
  | DecRoot of string * decTree * decTree
;;

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

(* Fonction : getVars
   Description : Retourne la liste des variables présentes dans une formule.
   Paramètre :
     - formula : La formule logique à analyser
   Retour : La liste des variables présentes dans la formule, triée par ordre croissant.
   Type : tformula -> string list *)
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
  List.sort_uniq compare (aux [] formula)
;;

(* Fonction : string_of_var
   Description : Convertit une variable tformula en sa représentation sous forme de chaîne de caractères.
   Paramètre :
     - var : La variable tformula à convertir
   Retour : La représentation de la variable en tant que chaîne de caractères.
   Type : tformula -> string *)
let rec string_of_var = function
  | Var v -> v
  | Value _ -> failwith "Invalid argument: Value"
  | Not _ -> failwith "Invalid argument: Not"
  | And _ -> failwith "Invalid argument: And"
  | Or _ -> failwith "Invalid argument: Or"
  | Implies _ -> failwith "Invalid argument: Implies"
  | Equivalent _ -> failwith "Invalid argument: Equivalent"
;;

(* --- test --- *)
(* getVars ex1 = ["P1";"P2";"Q1";"Q2"] *)
assert (getVars ex1 = [string_of_var p1; string_of_var p2; string_of_var q1; string_of_var q2]);;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 2 -------------------------- *)

(* Type d'environnement *)
type env = (string * bool) list

(* Fonction : evalFormula
   Description : Évalue une formule dans un environnement donné.
   Paramètres :
     - env : L'environnement contenant les valeurs des variables
     - formula : La formule logique à évaluer
   Retour : La valeur booléenne résultante de l'évaluation de la formule
   Type : env -> tformula -> bool *)
let rec evalFormula env formula =
  match formula with
  | Value b -> b
  | Var v -> List.assoc v env
  | Not f -> not (evalFormula env f)
  | And (f1, f2) -> evalFormula env f1 && evalFormula env f2
  | Or (f1, f2) -> evalFormula env f1 || evalFormula env f2
  | Implies (f1, f2) -> not (evalFormula env f1) || evalFormula env f2
  | Equivalent (f1, f2) -> evalFormula env f1 = evalFormula env f2;;

let env = [("P1", false); ("P2", false); ("Q1", false); ("Q2", false)]
(* evalFormula env ex1 = true *)
let () = assert (evalFormula env ex1 = true)

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 3 -------------------------- *)

(* Fonction : buildDecTree
   Description : Construit l'arbre de décision d'une formule.
   Paramètre :
     - formula : La formule logique à partir de laquelle construire l'arbre de décision
   Retour : L'arbre de décision correspondant à la formule
   Type : tformula -> decTree *)
let buildDecTree formula =
  let vars = getVars formula in
  let rec aux = function
    | [] -> invalid_arg "Empty variable list"
    | [v] -> DecRoot (v, DecLeaf true, DecLeaf false)
    | v :: rest ->
        let trueBranch = aux rest in
        let falseBranch = aux rest in
        DecRoot (v, trueBranch, falseBranch)
  in
  aux vars
;;

(* TODO : Marche pas *)
(* let tree = buildDecTree ex1 in
  let expected_tree =
    DecRoot ("P1",
      DecRoot ("P2",
        DecRoot ("Q1", DecRoot ("Q2", DecLeaf true, DecLeaf false),
          DecRoot ("Q2", DecLeaf false, DecLeaf true)),
        DecRoot ("Q1", DecRoot ("Q2", DecLeaf false, DecLeaf false),
          DecRoot ("Q2", DecLeaf false, DecLeaf false))),
      DecRoot ("P2",
        DecRoot ("Q1", DecRoot ("Q2", DecLeaf false, DecLeaf false),
          DecRoot ("Q2", DecLeaf false, DecLeaf false)),
        DecRoot ("Q1", DecRoot ("Q2", DecLeaf true, DecLeaf false),
          DecRoot ("Q2", DecLeaf false, DecLeaf true))))
    in
    assert (tree = expected_tree)
  ;; *)

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 4 -------------------------- *)


(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)
