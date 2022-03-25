(* Exercice 1 : *)

type 'a arbre = Vide 
              | Noeud of 'a  * 'a arbre * 'a arbre;;

let a6 = Noeud (6,Vide, Vide);;
let a5 = Noeud (5,a6, Vide);;
let a4 = Noeud (4,Vide, Vide);;
let a3 = Noeud (3,Vide,Vide);;
let a2 = Noeud (2,Vide, a5);;
let a1 = Noeud (1,a3, a4);;
let a = Noeud (9, a1, a2);; 

(* Question 1 *)
let rec profondeur = function
    Vide -> 0
  | Noeud(_,gauche,droite) ->1+max(profondeur gauche)(profondeur droite);;

profondeur(a);;

(* Question 2 *)
let rec existe cond = function
    Vide -> false
  | Noeud (x,gauche,droite) -> cond x || 
                               existe cond gauche ||
                               existe cond droite;;

existe (fun x -> x=4) (a);;
existe (fun x -> x=8) (a);;
existe (fun x -> x mod 2 = 0) (a);;

(* Question 3 *)
let rec map f = function
    Vide -> Vide
  | Noeud (x,gauche,droite) -> let gauche' = map f gauche and droite' = map f droite 
      in Noeud( (f x), gauche', droite');;

map (fun x -> string_of_int x) (a);;
map (fun x -> float_of_int x) (a);;

(* Question 4 *)
let rec sym a =
  match a with
  | Vide -> Vide
  | Noeud(fi,fg,fd) -> Noeud(fi , sym fd, sym fg);;

sym (a);;

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
