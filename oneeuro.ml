type lowPassFilter = { mutable firsttimelp: bool ; mutable hatxprev: float };;

type oneEuroFilter = { mutable firsttimeoe: bool;
	rate: float;
	mincutoff: float;
	beta: float;
	xfilt: lowPassFilter;
	dcutoff: float;
	dxfilt: lowPassFilter };;
	
let pi = 3.1415926;;

let alpha freq cutoff =
	let tau = 1.0 /. (2. *. pi *. cutoff)
	and te = 1.0 /. freq in
	1.0 /. (1.0 +. tau /. te);;
	
let lowpassfilter filter x alpha =
	if filter.firsttimelp then
	begin
		filter.firsttimelp <- false;
		filter.hatxprev <- x
	end;
	let hatx = alpha *. x +. (1.0 -. alpha) *. filter.hatxprev in
	filter.hatxprev <- hatx;
	hatx;;
	
let oneeurofilter filter x =
	let dx = 
	if (filter.firsttimeoe) then
	begin
		filter.firsttimeoe <- false;
		0.0
	end
	else
		(x -. filter.xfilt.hatxprev) *. filter.rate
	in
	let edx = lowpassfilter filter.dxfilt dx (alpha filter.rate filter.dcutoff) in
	let cutoff = filter.mincutoff +. filter.beta *. abs_float edx in
	lowpassfilter filter.xfilt x (alpha filter.rate cutoff);;

let createfilter freq mincutoff	beta dcutoff = { 
	firsttimeoe = true;
	rate = freq;
	mincutoff = mincutoff;
	beta = beta;
	xfilt = { firsttimelp = true ; hatxprev = 0.0 };
	dcutoff = dcutoff;
	dxfilt = { firsttimelp = true ; hatxprev = 0.0 } }
