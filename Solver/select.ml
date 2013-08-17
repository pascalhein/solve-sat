(* #load "up.cma";; *)
(* Chen Qi Kang, Groll Carolin, Neuwohner Meike, Ott Christiane*)
(* This module may read and use the internal state of the unit propagation
 * module, if not fragile *)

(* Returns the next variable with a proposed assignment for this variable. *)
(* get_next_variable : unit -> int;; *)

let get_next_variable () =
	let arr = (Up.get_assignment ()).Data.values in
	let i = ref 0 in
	let b = ref true in
	while (!b == true) do
		if (arr.(!i) == None) then
			b := false;
		i := (!i+1)
	done;
	!i
;;


		

(* Sets the random seed *)
(* set_random_seed : int -> unit;; *)

let set_random_seed gurkensalat = ();;
