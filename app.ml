(* --------------------------------------------------------------- *)
(* ------------------------Initialisation ------------------------ *)

open Printf (* Pour la partie bonus *)

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

let p1 : tformula = Var "P1";;
let p2 : tformula = Var "P2";;
let q1 : tformula = Var "Q1";;
let q2 : tformula = Var "Q2";;
let f1 : tformula = Equivalent (q1, q2);;
let f2 : tformula = Equivalent (p1, p2);;
let ex1 : tformula = And (f1, f2);;

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
let getVars : tformula -> string list = fun formula ->
  let rec aux : string list -> tformula -> string list = fun vars -> function
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
  List.sort_uniq String.compare (aux [] formula)
;;

(* Fonction : string_of_var
   Description : Convertit une variable tformula en sa représentation sous forme de chaîne de caractères.
   Paramètre :
     - var : La variable tformula à convertir
   Retour : La représentation de la variable en tant que chaîne de caractères.
   Type : tformula -> string *)
let string_of_var : tformula -> string = fun var ->
  match var with
  | Var v -> v
  | _ -> failwith "Invalid argument: string_of_var"
;;

(* --- test --- *)
assert (getVars ex1 = [string_of_var p1; string_of_var p2; string_of_var q1; string_of_var q2]);;
assert (getVars ex1 = ["P1"; "P2"; "Q1"; "Q2"]);;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 2 -------------------------- *)

(* Type d'environnement *)
type env = (string * bool) list;;

(* Fonction : evalFormula
   Description : Évalue une formule dans un environnement donné.
   Paramètres :
     - env : L'environnement contenant les valeurs des variables
     - formula : La formule logique à évaluer
   Retour : La valeur booléenne résultante de l'évaluation de la formule
   Type : env -> tformula -> bool *)
let evalFormula : (string * bool) list -> tformula -> bool = fun env formula ->
  let rec eval : tformula -> bool = function
    | Value b -> b
    | Var v -> List.assoc v env
    | Not f -> not (eval f)
    | And (f1, f2) -> eval f1 && eval f2
    | Or (f1, f2) -> eval f1 || eval f2
    | Implies (f1, f2) -> not (eval f1) || eval f2
    | Equivalent (f1, f2) -> eval f1 = eval f2
  in
  eval formula
;;

(* --- test --- *)
(* evalFormula env ex1 = true *)
let () = let env = [("P1", false); ("P2", false); ("Q1", false); ("Q2", false)] 
  in assert (evalFormula env ex1 = true);;

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
let buildDecTree : tformula -> decTree = fun formula ->
  let vars : string list = getVars formula in
  let rec aux (env : (string * bool) list) : string list -> decTree = function
    | [] -> DecLeaf (evalFormula env formula)
    | v :: rest ->
        let trueBranch : decTree = aux ((v, true) :: env) rest in
        let falseBranch : decTree = aux ((v, false) :: env) rest in
        DecRoot (v, trueBranch, falseBranch)
  in
  aux [] vars
;;

(* --- test --- *)
let tree = buildDecTree ex1 in
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
  ;; 
  
(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 4 -------------------------- *)
(* ------------- ( Ne fonctionne pas correctement ) -------------- *)

(* Type of BDD node *)
type bddNode =
  | BddLeaf of int * bool
  | BddNode of int * string * int * int
;;

(* Type of BDD *)
type bdd = int * (bddNode list);; (* un entier pour designer le noeud racine et la liste des noeuds *)

(* Fonction : buildBdd
   Description : Construit un BDD (Diagramme de Décision Binaire) à partir d'une formule logique.
   Paramètre :
     - formula : La formule logique à partir de laquelle construire le BDD
   Retour : Un couple (root_node, node_table) représentant le BDD, où :
            - root_node : L'identifiant du nœud racine du BDD
            - node_table : La liste des nœuds du BDD (triée dans l'ordre inverse de leur création)
   Type : tformula -> bdd *)
let buildBdd : tformula -> bdd = fun formula ->
  let node_table : bddNode list ref = ref [] in
  let node_count : int ref = ref 1 in

  let rec findNode (name : string) (l : bddNode list) : int option =
    match l with
    | [] -> None
    | BddNode (n, v, _, _) :: _ when v = name -> Some n
    | _ :: rest -> findNode name rest
  in

  let rec aux (vars : string list) (formula : tformula) : int =
    match formula with
    | Value b ->
        let leaf_node : bddNode = BddLeaf (!node_count, b) in
        node_table := leaf_node :: !node_table;
        node_count := !node_count + 1;
        !node_count - 1
    | Var v ->
        let var_node : bddNode = BddNode (!node_count, v, -1, -1) in
        node_table := var_node :: !node_table;
        node_count := !node_count + 1;
        !node_count - 1
    | Not f ->
        let sub_node : int = aux vars f in
        let neg_node : int =
          match findNode "not" !node_table with
          | Some n -> n
          | None ->
              node_count := !node_count + 1;
              let new_node : bddNode = BddNode (!node_count - 1, "not", sub_node, sub_node) in
              node_table := new_node :: !node_table;
              !node_count - 1
        in
        neg_node
    | And (f1, f2) ->
        let sub_node1 : int = aux vars f1 in
        let sub_node2 : int = aux vars f2 in
        let and_node : int =
          match findNode "and" !node_table with
          | Some n -> n
          | None ->
              node_count := !node_count + 1;
              let new_node : bddNode = BddNode (!node_count - 1, "and", sub_node1, sub_node2) in
              node_table := new_node :: !node_table;
              !node_count - 1
        in
        and_node
    | Or (f1, f2) ->
        let sub_node1 : int = aux vars f1 in
        let sub_node2 : int = aux vars f2 in
        let or_node : int =
          match findNode "or" !node_table with
          | Some n -> n
          | None ->
              node_count := !node_count + 1;
              let new_node : bddNode = BddNode (!node_count - 1, "or", sub_node1, sub_node2) in
              node_table := new_node :: !node_table;
              !node_count - 1
        in
        or_node
    | Implies (f1, f2) ->
        let sub_node1 : int = aux vars f1 in
        let sub_node2 : int = aux vars f2 in
        let implies_node : int =
          match findNode "implies" !node_table with
          | Some n -> n
          | None ->
              node_count := !node_count + 1;
              let new_node : bddNode = BddNode (!node_count - 1, "implies", sub_node1, sub_node2) in
              node_table := new_node :: !node_table;
              !node_count - 1
        in
        implies_node
    | Equivalent (f1, f2) ->
        let sub_node1 : int = aux vars f1 in
        let sub_node2 : int = aux vars f2 in
        let equiv_node : int =
          match findNode "equiv" !node_table with
          | Some n -> n
          | None ->
              node_count := !node_count + 1;
              let new_node : bddNode = BddNode (!node_count - 1, "equiv", sub_node1, sub_node2) in
              node_table := new_node :: !node_table;
              !node_count - 1
        in
        equiv_node
  in

  let vars : string list = getVars formula in
  let root_node : int = aux vars formula in
  (root_node, List.rev !node_table)
;;

(* --- test --- *)
let bdd = buildBdd ex1;;(* in
let expected_bdd =
  (10,
   [ BddNode (10, "P1", 8, 9);
     BddNode (9, "P2", 7, 5);
     BddNode (8, "P2", 5, 7);
     BddNode (7, "Q1", 6, 6);
     BddNode (6, "Q2", 2, 2);
     BddNode (5, "Q1", 3, 4);
     BddNode (4, "Q2", 2, 1);
     BddNode (3, "Q2", 1, 2);
     BddLeaf (2, false);
     BddLeaf (1, true) ])
in
assert(bdd = expected_bdd) *)

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 5 -------------------------- *)
(* -------- ( Ne fonctionne pas correctement sans la Q4 ) -------- *)

(* Fonction : simplifyBDD
   Description : Simplifie un BDD en éliminant les nœuds redondants.
   Paramètre :
     - bdd : Le BDD à simplifier
   Retour : Le BDD simplifié, représenté par un couple (root_node, updated_nodes), où :
            - root_node : L'identifiant du nœud racine du BDD simplifié
            - updated_nodes : La liste des nœuds du BDD simplifié (dans le même ordre que dans le BDD d'origine)
   Type : bdd -> bdd *)
let simplifyBDD : bdd -> bdd = fun bdd ->
  let root, nodes = bdd in
  let node_table = ref [] in

  let rec updateSucc (n : int) : int =
    match List.assoc_opt n !node_table with
    | Some next -> updateSucc next
    | None -> n
  in

  let rec buildTable = function
    | [] -> ()
    | node :: rest -> (
        match node with
        | BddNode (n, _, p, _) when p <> n ->
            let new_p = updateSucc p in
            node_table := (n, new_p) :: !node_table;
            buildTable rest
        | _ -> buildTable rest
      )
  in
  buildTable nodes;

  let updated_nodes =
    List.map (fun node ->
        match node with
        | BddNode (n, s, p, _) when p <> n ->
            let new_p = updateSucc p in
            BddNode (n, s, new_p, new_p)
        | _ -> node) nodes
  in

  (root, updated_nodes)
;;

(* --- test --- *)
let simplify_bdd = simplifyBDD bdd;;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 6 -------------------------- *)

(* Fonction : isTautology
   Description : Vérifie si une formule logique est une tautologie en utilisant un BDD simplifié.
   Paramètre :
     - formula : La formule logique à vérifier
   Retour : true si la formule est une tautologie, false sinon.
   Type : tformula -> bool *)
let isTautology : tformula -> bool = fun formula ->
  let bdd : bdd = buildBdd formula in
  let simplified_bdd : bdd = simplifyBDD bdd in
  let root, nodes = simplified_bdd in
  let rec traverse (node : bddNode) : bool =
    match node with
    | BddLeaf (_, b) -> b
    | BddNode (_, _, p, _) ->
        if p > 0 && p <= List.length nodes then
          traverse (List.nth nodes (p - 1))
        else
          false (* Index hors limite, retourne false *)
  in
  traverse (List.nth nodes (root - 1))
;;

(* --- test --- *)
let () = let formula_non_tautology = And (Var "p", Not (Var "p")) in
  let is_tautology = isTautology formula_non_tautology in
    assert (is_tautology = false)
;;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 7 -------------------------- *)

(* Fonction : areEquivalent
   Description : Vérifie si deux formules logiques sont équivalentes en comparant leurs BDD simplifiés.
   Paramètres :
     - formula1 : La première formule logique
     - formula2 : La deuxième formule logique
   Retour : true si les formules sont équivalentes, false sinon.
   Type : tformula -> tformula -> bool *)
let areEquivalent : tformula -> tformula -> bool = fun formula1 formula2 ->
  let bdd1 = buildBdd formula1 in
  let bdd2 = buildBdd formula2 in
  let simplifiedBDD1 = simplifyBDD bdd1 in
  let simplifiedBDD2 = simplifyBDD bdd2 in
  simplifiedBDD1 = simplifiedBDD2
;;

(* --- test --- *)
let () =
  assert (areEquivalent p1 p1 = true);
  assert (areEquivalent p1 p2 = false);
;;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 8 -------------------------- *)
(* ------------- ( Ne fonctionne pas correctement ) -------------- *)

(* Fonction : dotBDD
   Description : Génère une représentation Graphviz (DOT) d'un BDD.
   Paramètres :
     - filename : Le nom du fichier DOT à créer
     - bdd : Le BDD à représenter
   Effet de bord : Crée un fichier DOT contenant la représentation du BDD
   Type : string -> bdd -> unit *)
let dotBDD (filename : string) (bdd : bdd) : unit =
  let root, nodes = bdd in
  let file = open_out filename in
  let visited = Array.make (List.length nodes) false in

  let rec printNode (node : bddNode) : unit =
    match node with
    | BddLeaf (n, b) ->
        fprintf file "  %d [shape=ellipse, label=\"%d\", fontname=\"Arial\", fontweight=\"bold\"];\n" n n;
        fprintf file "  %d [style=bold, label=\"%s\"];\n" n (if b then "true" else "false")
    | BddNode (n, s, p, _) ->
        visited.(n - 1) <- true;
        visited.(p - 1) <- true;
        let edge_style = if p = n then "solid" else "dashed" in
        let edge_color = if p = n then "red" else "black" in
        fprintf file "  %d [label=\"%s\"];\n" n s;
        fprintf file "  %d -> %d [style=\"%s\", color=\"%s\"];\n" n p edge_style edge_color;
        let next_node = List.nth nodes (p - 1) in
        printNode next_node
  in

  fprintf file "digraph dotBDD {\n";
  printNode (List.nth nodes (root - 1));

  (* Add any remaining unvisited nodes *)
  List.iteri (fun i visited ->
      if not visited then (
        let n = i + 1 in
        fprintf file "  %d [shape=ellipse, label=\"%d\", fontname=\"Arial\", fontweight=\"bold\"];\n" n n
      )
    ) (Array.to_list visited);

  fprintf file "}\n";

  close_out file
;;

(* --- test --- *)
let expected_bdd : bdd = 
  (10,
  [ BddNode (10, "P1", 8, 9);
    BddNode (9, "P2", 7, 5);
    BddNode (8, "P2", 5, 7);
    BddNode (7, "Q1", 6, 6);
    BddNode (6, "Q2", 2, 2);
    BddNode (5, "Q1", 3, 4);
    BddNode (4, "Q2", 2, 1);
    BddNode (3, "Q2", 1, 2);
    BddLeaf (2, false);
    BddLeaf (1, true) ]) in
  dotBDD "src/dot/dotBDD.dot" expected_bdd
;;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)


(* --------------------------------------------------------------- *)
(* ------------------------- Question 9 -------------------------- *)

(* Fonction : dotDec
   Description : Génère un fichier au format DOT représentant un arbre de décision.
   Paramètres :
     - filename : Le nom du fichier DOT à générer
     - tree : L'arbre de décision à représenter
   Retour : Unit
   Effet de bord : Crée un fichier au format DOT représentant l'arbre de décision
   Type : string -> decTree -> unit *)
let rec dotDec (filename : string) (tree : decTree) : unit =
  let file = open_out filename in
  let write_line line = output_string file (line ^ "\n") in
  let counter = ref 0 in

  let rec traverse_node parent_label = function
    | DecLeaf b ->
        let node_label = "Leaf" ^ string_of_int !counter in
        counter := !counter + 1;
        write_line (node_label ^ " [label=\"" ^ string_of_bool b ^ "\", shape=box];");
        write_line (parent_label ^ " -> " ^ node_label ^ ";")
    | DecRoot (label, left, right) ->
        let node_label = "Node" ^ string_of_int !counter in
        counter := !counter + 1;
        write_line (node_label ^ " [label=\"" ^ label ^ "\"];");
        write_line (parent_label ^ " -> " ^ node_label ^ ";");
        traverse_node node_label left;
        traverse_node node_label right
  in

  write_line "digraph dotDec {";
  traverse_node "Root" tree;
  write_line "}";
  close_out file
;;

(* --- test --- *)
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
        
  in dotDec "src/dot/dotDec.dot" expected_tree
;;

(* --------------------------------------------------------------- *)
(* --------------------------------------------------------------- *)