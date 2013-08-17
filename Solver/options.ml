(* CLS, PH *)
(**
Option file for configurations in the SAT solver
**)

(**
loglevel determines the amount of logging output shown by the IO module
This constant should be changed to choose how much logging should be done on stdout.
**)
let log_error : int = 400;;
let log_warning : int = 300;;
let log_info : int = 200;;
let log_debug : int = 100;;
let log_all : int = 0;;

let loglevel = log_all;;
