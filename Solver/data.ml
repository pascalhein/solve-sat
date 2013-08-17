(* definitions of data structures
 *
 *)
 
(* responsible persons: Klaus Aehlig, Dimitri Scheftelowitsch *)

type clause = { the_clause : int list };;

type formula = { the_formula : clause array; num_vars : int };;

type assignment = { values : bool option array };;

let example_clauses = [| { the_clause = [ 1 ] };
                         { the_clause = [ -1 ; 2 ] };
						 { the_clause = [ -1 ; 3] };
						 { the_clause = [ -2; -3; 4 ] } |];;

let example_formula = { num_vars = 4; the_formula = example_clauses };;
