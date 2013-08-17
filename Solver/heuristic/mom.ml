(* Liste mit den Ausgangsklauseln *)
let formula = [|[|11;12|];[|12;12|];[|11;12|];[|21;22|];[|31;32|];[|-11;-21|];[|-11;-31|];[|-22;-32|];[|-22;-32|]|];;
(* Variable mit der groessten Nummer *)
let vars_number = (32+1);;


(* Dieses ist unsere Wahrheitsliste, in der wir angeben, ob eine Klausel bereits erfuellt *)
let truth_list = ref (Array.make (Array.length formula) None);;

(* Minimalste Laenge einer Klausel *)
let min_length = ref (Array.length formula.(0));;

(* Suche die minimale Laenge der Klauseln *)
let getMiniLength = 
	for i = 0 to ((Array.length formula)-1) do
		if ( ((Array.length (formula.(i))) <= !min_length) && (!truth_list.(i) == None) ) then
			min_length := (Array.length (formula.(i)));
	done
;;

(* -------- Variablen Counter ------- *)

(* Variable fuer Speicherung der Variablenlaenge, die in den kuerzesten Klauseln vorkomme *)
let variables_counter = Array.make vars_number 0;;
(* Variable fuer Speicherung der Klauseln der minimalsten Laenge*)
let min_clauses = ref [];;

let _ =
	let getMiniClausesVariables arr =
		begin
		(* Suche die Klauseln die die minmale Laenge entsprechen *)
		if ( (Array.length arr) == !min_length ) then
			begin		
			(* Speicher die Klauseln der minimale Laenge in eine Liste*)
			min_clauses := (Array.to_list arr) :: !min_clauses;
			
			(* Erhoehe den den Counter einer Variable um eins *)
			let getVariableFrequency var = 
				variables_counter.(abs var) <- (variables_counter.(abs var)+1)
			in
			(* Gehe die zweite Dimension der Formel durch*)
			Array.iter getVariableFrequency arr;				
			end
		end
	in
	(* Gehe die erste Dimension der Formel durch*)
    Array.iter getMiniClausesVariables formula
;;


(* ---------- Big Variable Search ------------*)

(* Anzahl der Variable die am haeufigsten vorkommt*)
let big_var_length = ref 0;;
(* Variable fuer Speicherung der Variablen die am haeugisten in den kleinsten Klauseln vorkommen*)
let big_variables_in_min_clauses = ref [];;


let _ = 
	(* Suche die maximale Variablen anzahl*)
	let getBigVarsLength var = 
		if (var > !big_var_length) then
			big_var_length := var
	in
	Array.iter getBigVarsLength variables_counter;
	
	let variable_counter_length = ((Array.length variables_counter)-1) in
	
	(* Suche die Variable die am Haufigsten vorkommt *)	
	for i = 0 to variable_counter_length do
		if (variables_counter.(i) == !big_var_length) then
			big_variables_in_min_clauses := i :: !big_variables_in_min_clauses;
	done
;;


(* ------ Man muss Prioritaeten setzen ---------*)

(*Speicherung der Prioritaeten*)
let priorities = ref [];;

(* arr[not neg count, neg count]*)
(* index = var *)
let literal_count = Array.make vars_number [||];;
for i = 0 to ((Array.length literal_count)-1) do
	literal_count.(i) <- [|0;0|];
done;;

(* Piroritaeten der Variablen festlegen die Am haeufigsten in den minimalsten Klauseln vorkommen *)
let _ =
	(*Wandle die Liste in ein Array*)
	let bvimc = Array.of_list !big_variables_in_min_clauses in
	
	(* Dursuche alle bvimc*)
	let bvimc_length = ( (Array.length bvimc)-1 ) in
	for a=0 to bvimc_length do	
		begin
		let formula_first_dimension_length = ( (Array.length formula)-1 ) in
		(* Durchsuche die 1.Dimension der Formula *)
		for b=0 to formula_first_dimension_length do
			begin
			let formula_second_dimension_length = ( (Array.length formula.(b))-1 ) in
			(* Durchsuche die 2.Dimension der Formula *)
			for c=0 to formula_second_dimension_length do
				begin
				(* Kommt die Variable in bvimc vor? *)
				if ( (abs formula.(b).(c)) == bvimc.(a) ) then
					begin
					(* Ist das Literal positiv? *)
					if(abs formula.(b).(c)) == formula.(b).(c) then	
						(* Erhoehung des not neg Counter, (index ist die variable) *)
						literal_count.(bvimc.(a)).(0) <- (literal_count.(bvimc.(a)).(0)+1)
					else
						(* Erhoehung des  neg Counter, (index ist die variable) *)
						literal_count.(bvimc.(a)).(1) <- (literal_count.(bvimc.(a)).(1)+1)
					end
				end
			done	
			end
		done
		end
	done
;;

(* Variable auswaehlen die die kleinste differenz zwischen neg und nicht neg hat*)

let _ =
	literal_count.(0).(0) <- 255;
	let difference_variable = ref 0 in
	for i = 0 to ((Array.length literal_count)-1) do
		if ( (literal_count.(i).(0) <> 0) || (literal_count.(i).(1) <> 0) ) then
			begin
			if ( (abs(literal_count.(i).(0) - literal_count.(i).(1))) < (abs ( (literal_count.(!difference_variable).(0)) - (literal_count.(!difference_variable).(1)) )) ) then
				difference_variable := i
			end
	done;
	!difference_variable
;;