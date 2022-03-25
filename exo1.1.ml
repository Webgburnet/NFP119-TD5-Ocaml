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