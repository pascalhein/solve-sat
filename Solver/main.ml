(*Xaver, Gunter, Daniel, Dominik*)

let example_clauses2 = [| {Data.the_clause = [1] };
                          {Data.the_clause = [-1;-2]} (* ; 
                          {Data.the_clause = [2] };
			  {Data.the_clause = [-2] }*) |] ;;

let example_formula2 = {Data.num_vars=2; Data.the_formula = example_clauses2 } ;; 


let set_and_analyze next_variable = if Up.set_variable next_variable then true
                                    else (Up.analyze_conflict () ; false) ;;

let is_satisfiable formula =
  let rec dpll () = 
    if Up.get_num_set () = formula.Data.num_vars then true else
      (*some vars unassigned: choose next var*)
      let next_variable = Select.get_next_variable () in 
        if set_and_analyze next_variable & dpll () then 
          true
        else (
          Up.unset_variable next_variable ;
          if  set_and_analyze (-next_variable) & dpll () then
            true
          else (
	    Up.unset_variable (-next_variable) ;
	    false
	  )
        ) in dpll () ;;


let the_solver file_name =
  let formula = Io.read_formula file_name in
    Io.comment Options.log_info "Formula parsed" ;
    let _ = Up.init formula in
      Io.comment Options.log_info "UP initialized" ;
      if (is_satisfiable formula) then
        Io.satisfiable (Up.get_assignment ())
      else Io.unsatisfiable ();;

let _ = if (Array.length Sys.argv > 1) then ( 
  if (Array.length Sys.argv > 2) then 
    Select.set_random_seed (int_of_string (Array.get Sys.argv 2)) ;
    the_solver (Array.get Sys.argv 1)
)
else () ;
