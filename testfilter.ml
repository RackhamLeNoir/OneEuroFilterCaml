#use "oneeuro.ml";;
open Graphics;;

let parabole a b c = 
  let rec tr = function
    | 0 -> []
    | n -> let y = a *. float_of_int(n * n) +. b *. float_of_int(n) +. c in
	      y :: (tr (n - 1))
  in tr;;

let drawlist l =
	let nb = List.length l in
	let rec aux n = function
	| [] -> ()
	| x :: l' -> if (x >= 0.0 && x < 600.0) then
			plot n (int_of_float(x));
		aux (n - 1) l'
	in aux nb l;;

let rec noisify range = function
	| [] -> []
	| x :: l -> (x +. Random.float range -. range /. 2.0) :: noisify range l;;

let rec filtered filter = function
	| [] -> []
	| x :: l -> (oneeurofilter filter x) :: (filtered filter l);;

let p = parabole (-.0.002) 1.0 400.0 800;;
let p' = noisify 50.0 p;;
let f = createfilter 100.0 1.0 0.0 1.0;;
let p'' = filtered f p';;

open_graph " 800x600";;
clear_graph();;	

set_color black;;
drawlist p;;

set_color red;;
drawlist p';;

set_color blue;;
drawlist p'';;

