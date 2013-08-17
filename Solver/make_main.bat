ocamlc -c data.ml
ocamlc -a -o data.cma data.cmo
ocamlc -c io.ml
ocamlc -a -o io.cma io.cmo
ocamlc -c up.ml
ocamlc -a -o up.cma up.cmo
ocamlc -c select.ml
ocamlc -a -o select.cma select.cmo
ocamlfind ocamlc -g -o solver.exe -package str -linkpkg data.cma io.cma up.cma select.cma  main.ml 
