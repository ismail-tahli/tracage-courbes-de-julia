open Graphics;;
open Complex;;


(* Type config avec les elements configurables P *)
type config = {
  mutable c:Complex.t;
  mutable r:int;
  mutable z:float;
  mutable borne: int;
  mutable size : int;
}

(* Type command S *)
type command =
ChangeComplex of Complex.t
|ChangeRepere of int
|ChangeZoom of float
|ChangeBorne of int
|ResizeWindow of int
;;

(*Fonction Check : string->float->command*)

let check s n= match s with
"ChangeRepere" -> ChangeRepere (int_of_float n)
|"ChangeZoom" -> ChangeZoom n
|"ChangeBorne" -> ChangeBorne (int_of_float n)
|"ResizeWindow" -> ResizeWindow (int_of_float n)
|"ChangeComplex" -> let m = read_float() in let c={Complex.re=n;Complex.im=m} in ChangeComplex c
|_ -> failwith "Erreur Command !"
;;

(* Résolu : !Pour Changer le nombre complex il faut deux entrées %f mais pour les autres commandes il nous faut qu'une seule entrée !*)


let read_command() = Scanf.sscanf (read_line()) "%s %f" (fun x y->check x y);;

(* La fonction openf pour éviter la duplication de code*)

let openf conf = open_graph (" "^(string_of_int conf.size)^"x"^ (string_of_int conf.size));;

let execute conf com = 
let exec conf com = match com with
ChangeRepere n -> conf.r <- n
|ChangeZoom n -> conf.z <- n
|ChangeBorne n -> conf.borne <- n
|ResizeWindow n -> conf.size <- n
|ChangeComplex c -> conf.c <- c
in
exec conf com;
close_graph(); (* Fermuture de la fenetre pour que le changement de la dimension s'applique. *)
(* Ouverture de la fenetre avec la nouvelle dimension*)
openf conf; 
conf;; (* Renvoie la nouvelle configuration*)

(*Teste si la norme est supérieur à 2 avec l'emploi d'une fonction récursive terminale*)
let lvl_of conf p= let rec aux conf p acc=
if ((Complex.norm p) > 2.0) then acc
else match acc with
|n when n=conf.borne -> conf.borne
|m -> let p' = Complex.add (Complex.mul p p) conf.c in
aux conf p' (acc+1)
in aux conf p 0
             ;;

(* Dessine chaque pixel de la zone de dessin avec le niveau de gris correspondant avec l'emploi de deux fonctions récursives terminales une pour les abscisses et l'autre pour les ordonnées.*)

let julia config =
clear_graph ();
let rec loopx i = if i<=config.r then
let rec loopy j = if j<=config.r then 
let p={Complex.re=((float_of_int i) /. config.z);Complex.im=((float_of_int j) /. config.z)}
in let k =((lvl_of  config p)*10) (* ! La multiplication par 10 est optionnelle, Pour augumenter le niveau de gris *)
in set_color (rgb (255-k) (255-k) (255-k)); 
(* in set_color (rgb (191-k/2) (191-k/2) (191-k)); *) (*191 est pas 255 parce que sur l'énoné est marqué niveau de gris.*)
plot ((config.size/2)+i) ((config.size/2)+j);
loopy (j + 1);
else loopx (i +1)
in loopy ((-1)*config.r);
in loopx ((-1)*config.r);;

(* Fonction récursive terminale qui effectue le changement selon la commande entrée.*)

let rec read_command_and_draw conf = 
let c =read_command()
in let nconf =execute conf c
in julia nconf;
read_command_and_draw nconf
;;

(*La fonction principale*)
let main conf=
(*Overture de la fenetre*)
openf conf;
julia conf;
read_command_and_draw conf;;

(*La configuration par défaut du programme elle peut être modifiée.*)

let configuration = {c={Complex.re=(-0.4);Complex.im=(-0.6)};r=300;z=200.;borne=256;size=600;};;

main configuration;;

(* Pour ne pas fermer la fenetre directement après l'execution du programme.*)
ignore (read_key ());;