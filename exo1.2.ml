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

(* Question 2 *)
let rec existe cond = function
    Vide -> false
  | Noeud (x,gauche,droite) -> cond x || 
                               existe cond gauche ||
                               existe cond droite;;

existe (fun x -> x=4) (a);;
existe (fun x -> x=8) (a);;
existe (fun x -> x mod 2 = 0) (a);;
