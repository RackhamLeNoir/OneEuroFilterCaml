type oneEuroFilter;;

(*
	oneeurofilter filter value
	returns the value fintered with the given filter
*)
val oneeurofilter: oneEuroFilter -> float -> float;;

(*
	createfilter freq mincutoff beta dcutoff
	
	Creates a filter with these parameters:
	freq: 
	mincutoff	
	beta 
	dcutoff
*)
val createfilter: float -> float -> float -> float -> oneEuroFilter;;