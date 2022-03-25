(* Exercice 2 : *)

type noeud = Node of char * noeud list;;
type dico = noeud list;;
let d7:dico = [Node('t',[])];;
let d6:dico = [Node('c',[]);Node('l',[Node('l',[Node('e',[])])])];;
let d5:dico = [Node('e',d6)];;
let d4:dico = [Node('x',[])];;
let d3:dico = [Node('b',[Node('r',[Node('e',[])])])];;
let d2:dico = [Node('r',d3@d7);Node('u',d4);Node('s',[])];;
let d1:dico = [Node('a',d2);Node('b',d5)];;


type noeud = Node of char * noeud list;;
type dico = noeud list;;

(* [dico_assoc c d] rend le noeud du dictionnaire [d] ayant le
   caractere [c] au sommet.
   @raise Not_found si un tel noeud n’existe pas dans [d]. *) 
let dico_assoc (c:char) (d:dico) : noeud =
  List.find (fun nd -> let Node (c',_) = nd in c=c') d;;

(*[existe_i d m i] rend true si la chaˆıne de caract`ere [m] ∗ a partir
du [i]eme caractere ∗ appartient au dictionnaire [d]. Il s’agit
d’une fonction auxiliaire `a existe. On aurait pu la definir
localement a existe.*)
let rec existe_i (d:dico) (m:string) (i:int) : bool =
  try
    let c = String.get m i in
    let nd_avec_c_au_sommet = dico_assoc c d in
    let Node(_,sous_dico) = nd_avec_c_au_sommet in
    existe_i sous_dico m (i+1)
  with
  | Not_found -> false (* on n’a pas trouve de noeud avec c au sommet.*)
  | Invalid_argument _ -> true;; (* On est arrive au bout du mot.*)
      
(* [existe d m] rend true si la chaine de caractere [m] appartient au
   dictionnaire [d].*)
let existe (d:dico) (m:string) : bool =
  existe_i d m 0 ;; (*On se content d’appeler existe i en commen ̧cant `a 0. *)   
                   
(* TESTS *)
let d7:dico = [Node('t',[])];;
let d6:dico = [Node('c',[]);Node('l',[Node('l',[Node('e',[])])])];;
let d5:dico = [Node('e',d6)];;
let d4:dico = [Node('x',[])];;
let d3:dico = [Node('b',[Node('r',[Node('e',[])])])];;
let d2:dico = [Node('r',d3@d7);Node('u',d4);Node('s',[])];;
let d1:dico = [Node('a',d2);Node('b',d5)];;

existe d1 "aux";;
existe d1 "auxi";;
existe d1 "au";;
existe d1 "bell";;
existe d1 "belle";;
existe d1 "bclle";;