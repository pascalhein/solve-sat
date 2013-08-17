(**
make_lookup
remove_pos_neg
remove_same
simplify
init
set_variable
has_no_conflict
test_unit
**)

open OUnit;;
open Up;;

let test_abs_1 () = assert_equal 1 (abs 1);;
let test_abs_2 () = assert_equal 2 (abs (-2));;
let test_abs_3 () = assert_equal 0 (abs 0);;

let test_make_signed_1 () = assert_equal 3 (make_signed (3, true));;
let test_make_signed_2 () = assert_equal (-7) (make_signed (7, false));;
let test_make_signed_3 () = assert_equal 0 (make_signed (0, false));;

let test_separate_signed_1 () = assert_equal (1, true) (separate_signed 1);;
let test_separate_signed_2 () = assert_equal (2, false) (separate_signed (-2));;
let test_separate_signed_3 () = assert_equal (3, true) (separate_signed 3);;

let test_reasons () =
	init Data.example_formula;
	assert_equal ~msg:"1" [| []; []; []; []; [] |] !reasons;
	add_reasons 3 [1; 2];
	assert_equal ~msg:"2" [| []; []; []; [1; 2]; [] |] !reasons;
	assert_equal ~msg:"3" [1; 2] (get_reasons 3);
	assert_equal ~msg:"4" [] (get_reasons 2);
	clear_reasons 3;
	assert_equal ~msg:"5" [] (get_reasons 3);
	add_reasons 4 [1];
	add_reasons 2 [3];
	assert_equal ~msg:"6" [| []; []; [3]; []; [1] |] !reasons;
	;;

let test_implications () =
	init Data.example_formula;
	assert_equal ~msg:"1" [| []; []; []; []; [] |] !implications;
	add_implication 3 7;
	assert_equal ~msg:"2" [| []; []; []; [7]; [] |] !implications;
	add_implication 3 9;
	assert_equal ~msg:"3" [| []; []; []; [9;7]; [] |] !implications;
	add_implication 2 7;
	assert_equal ~msg:"4" [7] (get_implications 2);
	assert_equal ~msg:"5" [9;7] (get_implications 3);
	assert_equal ~msg:"6" [] (get_implications 4);
	remove_implication 3 9;
	remove_implication 3 6;
	assert_equal ~msg:"7" [7] (get_implications 3);
	;;

let test_init () =
	init Data.example_formula;
	assert_equal ~msg:"1" !num_vars 4;
	assert_equal ~msg:"2" !formula Data.example_formula.Data.the_formula;
	assert_equal ~msg:"3" (Array.length !values) 5;
	assert_equal ~msg:"4" (Array.length !implications) 5;
	assert_equal ~msg:"5" (Array.length !reasons) 5
	;;

let test_is_satisfiable () =
	init Data.example_formula;
	!values.(1) <- None;
	assert_equal ~msg:"1" true (is_satisfiable { Data.the_clause = [1] });
	!values.(1) <- Some true;
	assert_equal ~msg:"2" true (is_satisfiable { Data.the_clause = [1] });
	!values.(1) <- Some false;
	assert_equal ~msg:"3" false (is_satisfiable { Data.the_clause = [1] });
	!values.(1) <- None;
	assert_equal ~msg:"4" true (is_satisfiable { Data.the_clause = [-1] });
	!values.(1) <- Some true;
	assert_equal ~msg:"5" false (is_satisfiable { Data.the_clause = [-1] });
	!values.(1) <- Some false;
	assert_equal ~msg:"6" true (is_satisfiable { Data.the_clause = [-1] });
	!values.(1) <- None;
	!values.(2) <- None;
	assert_equal ~msg:"7" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- None;
	!values.(2) <- Some true;
	assert_equal ~msg:"8" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- None;
	!values.(2) <- Some false;
	assert_equal ~msg:"9" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some true;
	!values.(2) <- None;
	assert_equal ~msg:"10" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some true;
	!values.(2) <- Some true;
	assert_equal ~msg:"11" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some true;
	!values.(2) <- Some false;
	assert_equal ~msg:"12" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some false;
	!values.(2) <- None;
	assert_equal ~msg:"13" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some false;
	!values.(2) <- Some true;
	assert_equal ~msg:"14" false (is_satisfiable { Data.the_clause = [1; (-2)] });
	!values.(1) <- Some false;
	!values.(2) <- Some false;
	assert_equal ~msg:"15" true (is_satisfiable { Data.the_clause = [1; (-2)] });
	;;

let test_has_no_conflict () =
	init Data.example_formula;
	formula := [| { Data.the_clause = [1] } |];
	assert_equal ~msg:"1" true (has_no_conflict 1);
	!values.(1) <- Some false;
	!values.(2) <- Some true;
	assert_equal ~msg:"2" false (has_no_conflict 1);
	!values.(1) <- Some true;
	!values.(2) <- Some false;
	assert_equal ~msg:"3" true (has_no_conflict 1);
	init Data.example_formula;
	formula := [| { Data.the_clause = [1; (-2)] } |];
	assert_equal ~msg:"4" true (has_no_conflict 1);
	!values.(1) <- Some false;
	!values.(2) <- Some true;
	assert_equal ~msg:"4" false (has_no_conflict 2);
	!values.(1) <- Some true;
	!values.(2) <- Some false;
	assert_equal ~msg:"6" true (has_no_conflict 1);
	;;

let test_analyze_conflict () =
	init Data.example_formula;
	storage := [(Some (1,[2])); (Some (-2,[3]))];
	analyze_conflict ();
	assert_equal [] !storage;
	;;

let test_get_assignment () =	
	init Data.example_formula;
	assert_equal ~msg:"1" [| None; None; None; None |] (get_assignment ()).Data.values;
	assert_equal ~msg:"2" 0 (get_num_set ());
	!values.(1) <- Some true;
	!values.(4) <- Some false;
	assert_equal ~msg:"3" [| Some true; None; None; Some false |] (get_assignment ()).Data.values;
	assert_equal ~msg:"4" 2 (get_num_set ());
	;;

let test_set_variable () =
	init Data.example_formula;
	formula := [| { Data.the_clause = [1; (-2)] } |];
	assert_equal ~msg:"1" true (set_variable 1);
	assert_equal ~msg:"2" [| Some true; None; None; None |] (get_assignment ()).Data.values;
	assert_equal ~msg:"3" true (set_variable (-1));
	assert_equal ~msg:"4" [| Some false; Some false; None; None |] (get_assignment ()).Data.values;
	assert_equal ~msg:"5" true (set_variable (-2));
	assert_equal ~msg:"6" [| Some false; Some false; None; None |] (get_assignment ()).Data.values;
	assert_equal ~msg:"7" false (set_variable 2);
	assert_equal ~msg:"8" [| Some false; Some true; None; None |] (get_assignment ()).Data.values;
	init { Data.num_vars = 4; Data.the_formula = [| { Data.the_clause = [1; (-2)] }; { Data.the_clause = [2; 3] }; { Data.the_clause = [3; 4] } |] };
	assert_equal ~msg:"9" true (set_variable (-3));
	assert_equal ~msg:"10" [| Some true; Some true; Some false; Some true |] (get_assignment ()).Data.values;
	assert_equal ~msg:"11" [4; 2] (get_implications 3);
	assert_equal ~msg:"12" [1] (get_implications 2);
	assert_equal ~msg:"13" [-2] (get_reasons 1);
	assert_equal ~msg:"14" [3] (get_reasons 2);
	;;

let test_unset_variable () =
	init Data.example_formula;
	formula := [| { Data.the_clause = [1; (-2)] } |];
	values := [| None; Some true; None; Some false; Some true |];
	unset_variable 1;
	assert_equal ~msg:"1" [| None; None; None; Some false; Some true |] !values;
	unset_variable (-3);
	assert_equal ~msg:"2" [| None; None; None; None; Some true |] !values;
	unset_variable (-4);
	assert_equal ~msg:"3" [| None; None; None; None; None |] !values;
	init { Data.num_vars = 4; Data.the_formula = [| { Data.the_clause = [1; (-2)] }; { Data.the_clause = [2; 3] }; { Data.the_clause = [3; 4] } |] };
	set_variable (-3);
	unset_variable 3;
	assert_equal ~msg:"4" [| None; None; None; None; None |] !values;
	assert_equal ~msg:"5" [| []; []; []; []; [] |] !implications;
	assert_equal ~msg:"6" [| []; []; []; []; [] |] !reasons;
	;;

let test_test_unit () =
	init { Data.num_vars = 3; Data.the_formula = [| { Data.the_clause = [1; 2] } |] };
	!values.(1) <- Some false;
	assert_equal ~msg:"1" (Some (2,[1])) (test_unit { Data.the_clause = [1; 2] });
	!values.(1) <- Some true;
	assert_equal ~msg:"2" None (test_unit { Data.the_clause = [1; 2] });
	assert_equal ~msg:"3" (Some (2,[-1])) (test_unit { Data.the_clause = [-1; 2] });
	assert_equal ~msg:"4" (Some (2,[])) (test_unit { Data.the_clause = [2] });
	assert_equal ~msg:"5" (Some (-2,[])) (test_unit { Data.the_clause = [-2] });
	assert_equal ~msg:"6" None (test_unit { Data.the_clause = [-1; 2; -3] });
	;;

let test_find_unit () =
	init { Data.num_vars = 4; Data.the_formula = [| { Data.the_clause = [1; 2] }; { Data.the_clause = [3; -4] } |] };
	!values.(3) <- Some false;
	assert_equal ~msg:"1" (Some (-4,[3])) (find_unit 4);
	assert_equal ~msg:"2" [] !storage;
	init { Data.num_vars = 4; Data.the_formula = [| { Data.the_clause = [1; (-2)] }; { Data.the_clause = [2; 3] }; { Data.the_clause = [3; 4] } |] };
	!values.(3) <- Some false;
	assert_equal ~msg:"3" (Some (2,[3])) (find_unit 4);
	assert_equal ~msg:"4" [(Some (4,[3]))] !storage;
	!values.(2) <- Some true;
	assert_equal ~msg:"5" (Some (4,[3])) (find_unit 4);
	assert_equal ~msg:"6" [] !storage;
	!values.(4) <- Some true;
	assert_equal ~msg:"7" (Some (1,[-2])) (find_unit 4);
	assert_equal ~msg:"8" [] !storage;
	!values.(1) <- Some true;
	assert_equal ~msg:"9" None (find_unit 4);
	;;

let up = "UP" >::: [
	"test_abs_1" >:: test_abs_1;
	"test_abs_2" >:: test_abs_2;
	"test_abs_3" >:: test_abs_3;
	"test_make_signed_1" >:: test_make_signed_1;
	"test_make_signed_2" >:: test_make_signed_2;
	"test_make_signed_3" >:: test_make_signed_3;
	"test_separate_signed_1" >:: test_separate_signed_1;
	"test_separate_signed_2" >:: test_separate_signed_2;
	"test_separate_signed_3" >:: test_separate_signed_3;
	"test_init" >:: test_init;
	"test_is_satisfiable" >:: test_is_satisfiable;
	"test_has_no_conflict" >:: test_has_no_conflict;
	"test_analyze_conflict" >:: test_analyze_conflict;
	"test_get_assignment" >:: test_get_assignment;
	"test_set_variable" >:: test_set_variable;
	"test_reasons" >:: test_reasons;
	"test_implications" >:: test_implications;
	"test_test_unit" >:: test_test_unit;
	"test_find_unit" >:: test_find_unit;
	];;

let _ =	run_test_tt_main up;;
