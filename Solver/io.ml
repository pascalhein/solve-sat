(* MPE, AP, PS, MH *)
(* May be of help: Scanf.* *)

(* Reads a formula into an internal representation *)
(* read_formula : string -> Data.formula;; *)

(* Prints the solution line for unsatisfiable formulas and exits the program
 * with the correct exit code. Is called when the formula is found to be
 * unsatisfiable
 *)
(* unsatisfiable : unit -> unit;; *)

(* Prints the solution line for satisfiable formulas and exits the program
 * with the correct exit code. Is called when the formula is found to be
 * satisfiable. The argument is a satisfying assignment.
 *)
(* satisfiable : Data.assignment -> unit;; *)

(* comment : string -> unit;; *)


let read_formula inPath =
  let inChannel = open_in inPath in
  let run = ref true in
  let numVars = ref 0 in
  let clauses = ref (Array.make 0 {Data.the_clause = []}) in
  let forBound = ref 0 in
  
  while !run do
	let inLine = input_line inChannel in
	let splitStr = Str.split (Str.regexp "[ \t]+") inLine in
	
	(* List.iter (Printf.printf "%s \n") splitStr;
	Printf.printf "%d \n" (List.length splitStr); *)
	  
	if ((List.hd splitStr) = "p") then
	(
	  (* Printf.printf "Debug List %s \n" (List.hd splitStr); *)
	  numVars := int_of_string (List.hd (List.tl (List.tl splitStr)));
	  (* Printf.printf "%d \n" !numVars; *)
	  forBound := (int_of_string (List.hd (List.tl(List.tl (List.tl splitStr)))) );
	  clauses := Array.make !forBound { Data.the_clause = [0]};
	  run := false;
	) else () ;
  done;
  for i = 0 to ((!forBound) - 1) do
	let inLine = input_line inChannel in
	let splitStr = ref (Str.split (Str.regexp "[ \t]+") inLine) in
	let clauseList = ref [] in
	 
	while (List.length !splitStr > 1) do
	  clauseList := (int_of_string (List.hd !splitStr)) :: !clauseList;
	  splitStr := (List.tl !splitStr);
	done;
	clauseList := List.rev !clauseList;
	!clauses.(i) <- { Data.the_clause = !clauseList };
  done;
  close_in inChannel;
  let formula = { Data.the_formula = !clauses ; num_vars = !numVars} in
  formula
  (* with e ->
    close_in_no_err inChannel;
	raise e; *)
;;
let unsatisfiable () =
  Printf.printf "s UNSATISFIABLE \n" ;
  exit 20
;;

let comment level message =
	if level >= Options.loglevel then
		Printf.printf "c %s \n%!" message  
	;;

let satisfiable solveAssignment =
  Printf.printf "s SATISFIABLE \n";
  for i = 1 to ((Array.length solveAssignment.Data.values)) do
    Printf.printf("v ");
	if solveAssignment.Data.values.(i-1) != None then
	(
	  if solveAssignment.Data.values.(i-1) = Some true then
	  (
	    Printf.printf "%d \n" i;
	  )
	  else
	  (
	    Printf.printf "%d \n" (i * -1);
	  )
	)
	else
	(
	)
  done;
  exit 10
;;
