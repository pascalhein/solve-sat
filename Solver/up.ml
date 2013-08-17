(* Christian, Stefan, Tobias, Pascal *)
(* #load "data.cma";; *)

(* Utilities block *)

let abs (x : int) : int =
	if x < 0 then -x else x
	;;

let make_signed ((i, positive) : (int * bool)) : int =
	i * (if positive then 1 else -1)
	;;

let separate_signed (i : int) : (int * bool) =
	if i < 0 then (-i, false) else (i, true)
	;;

(* End Utilities *)

let values : bool option array ref = ref [||];;
let formula : Data.clause array ref = ref [||];;
let num_vars : int ref = ref 0;;
let implications : int list array ref = ref [||];;
let reasons : int list array ref = ref [||];;
let storage : (int * (int list)) option list ref = ref [];;
let lookup : int list array ref = ref [||];;

(* Data access *)

(* get_assignment : unit -> Data.assignment;; *)

let get_assignment () : Data.assignment =
	{ Data.values = Array.sub !values 1 !num_vars }
	;;

let get_num_set () : int =
	let count = List.length (List.filter (fun x -> x<>None) (Array.to_list !values)) - 1 in
	Io.comment Options.log_debug (Printf.sprintf "UP: get_num_set called. Currently %d set variables" count);
	count
    ;;

let add_implication (origin : int) (consequence : int) : unit =
	!implications.(abs origin) <- (abs consequence) :: !implications.(abs origin)
	;;

let get_implications (origin : int) : int list =
	!implications.(abs origin)
	;;

let remove_implication (origin : int) (consequence : int) : unit =
	!implications.(abs origin) <- List.filter (fun x -> x <> consequence) !implications.(abs origin)
	;;

let add_reasons (consequence : int) (causes : int list) : unit =
	!reasons.(abs consequence) <- causes	
	;;

let get_reasons (consequence : int) : int list =
	!reasons.(abs consequence)
	;;

let clear_reasons (consequence : int) : unit =
	!reasons.(abs consequence) <- []
	;;

(* End Data access *)

let make_lookup () : unit =
	lookup := Array.make (!num_vars + 1) [];
	for i = 0 to (Array.length !formula - 1) do
		List.iter (fun l ->
			!lookup.(abs l) <- List.append !lookup.(abs l) [(if (snd (separate_signed l)) then 1 else (-1)) * (i + 1)]
			) !formula.(i).Data.the_clause
	done
	;;

let rec remove_pos_neg (c : Data.clause) : Data.clause =
	let l = ref c.Data.the_clause in
	let i = ref 0 in
	while !i < (List.length !l) do
		let el = List.nth !l !i in
		if (List.mem (-el) !l) then
		(
			Io.comment Options.log_all (Printf.sprintf "UP: remove_pos_neg: removing %d" el);
			l := List.filter (fun x -> (abs x) <> (abs el)) !l;
			i := List.length !l;
			l := (remove_pos_neg { Data.the_clause = !l }).Data.the_clause;
		);
		i := !i + 1
	done;
	if List.length !l = 0 then l := [0];
	{ Data.the_clause = !l }
	;;

let rec remove_same (c : Data.clause) : Data.clause =
	let l = ref c.Data.the_clause in
	let i = ref 0 in
	while !i < (List.length !l) do
		let el = List.nth !l !i in
		if (List.length (List.filter (fun x -> x = el) !l) > 1) then
		(
			Io.comment Options.log_all (Printf.sprintf "UP: remove_same: removing %d" el);
			l := List.filter (fun x -> x <> el) !l;
			l := List.append !l [el];
			i := List.length !l;
			l := (remove_same { Data.the_clause = !l }).Data.the_clause;
		);
		i := !i + 1
	done;
	{ Data.the_clause = !l }
	;;

let simplify () : unit =
	formula := Array.map remove_pos_neg !formula;
	formula := Array.map remove_same !formula
	;;

(* Copies the formula into an internal static variable that contains an
 * appropriate representation of the current state of set and unset variables.
 * May add additional internal state if needed for efficient implementation
 *)
(* init : Data.formula -> unit *)
let init (x : Data.formula) : unit =
	formula := x.Data.the_formula;
	num_vars := x.Data.num_vars;
	values := (Array.make (!num_vars + 1) None);
	implications := (Array.make (!num_vars + 1) []);
	reasons := (Array.make (!num_vars + 1) []);
	!values.(0) <- Some true;
	simplify ();
	make_lookup ()
	;;

let is_satisfiable (c : Data.clause) : bool =
	List.exists (fun lit -> ((!values.(abs lit) = None) || (!values.(abs lit) = Some (lit >= 0)))) c.Data.the_clause
	;;

let has_no_conflict (x : int) : bool =
(**	Array.fold_left (fun iv cur -> iv && (is_satisfiable cur)) true (!formula) **)
	Io.comment Options.log_debug (Printf.sprintf "UP: has_no_conflict %d" x);
	let clauses = List.map (fun c -> !formula.((abs c) - 1)) !lookup.(abs x) in
	List.fold_left (fun iv cur -> iv && (is_satisfiable cur)) true clauses
	;;

let test_unit (c : Data.clause) : (int * (int list)) option =
	if List.length (List.filter (fun lit -> (!values.(abs lit) = None)) c.Data.the_clause) <> 1 then None
	else if List.length (List.filter (fun lit -> (!values.(abs lit) = Some (lit > 0))) c.Data.the_clause) > 0 then None
	else
	(
		Some (List.hd (List.filter (fun lit -> (!values.(abs lit) = None)) c.Data.the_clause), List.filter (fun lit -> (!values.(abs lit) <> None)) c.Data.the_clause)
	)
	;;

let rec find_unit (varname : int) : (int * (int list)) option =
	if (List.length !storage > 0) then
	(
		let h = List.hd !storage in
		storage := List.tl !storage;
		match h with
			  None -> None;
			| Some y ->
				if !values.(abs (fst y)) <> None then (find_unit varname) else h
	)
	else 
	(
		let clauses = List.map (fun c -> !formula.((abs c) - 1)) !lookup.(abs varname) in
		storage := List.filter (fun x -> x <> None) (List.map test_unit clauses);
		if (List.length !storage = 0) then None
		else
		let h = List.hd !storage in
		storage := List.tl !storage;
		h
	)
	;;

(* sets a variable, propagates everything
 * return value: true if successful
 *               false if conflict
 *)

let rec set_variable_internal (x : int) : bool =
	!values.(abs x) <- Some (x > 0);
	if not (has_no_conflict x) then false else
	(
	(*	find any unit tuple
		set_variable y
		foreach literal l in causing clause:
			make implications from l.var to y
			make reasons to y.var from l*)
		let tuple = find_unit x in
		match tuple with
			  None -> true
			| Some y ->
				List.iter (fun l -> add_implication (abs l) (fst y)) (snd y);
				add_reasons (abs (fst y)) (snd y);
				set_variable_internal (fst y)
	)
	;;
	

(* set_variable : int -> bool;; *)
let set_variable (x : int) : bool =
	Io.comment Options.log_info (Printf.sprintf "UP: set_variable is setting %d externally" x);
	set_variable_internal x
	;;

(* analyze_conflict : unit -> unit;; *)
let analyze_conflict () : unit =
	storage := []
	;;

let rec unset_variable_internal (x : int) : unit =
	!values.(abs x) <- None;
	List.iter unset_variable_internal (get_implications x);
	List.iter (fun y -> remove_implication y x) (get_reasons x);
	clear_reasons x
	;;

(* unset_variable : int -> unit;; *)
let unset_variable (x : int) : unit =
	Io.comment Options.log_info (Printf.sprintf "UP: unset_variable is removing conflict due to variable %d externally" x);
	unset_variable_internal x
	;;
