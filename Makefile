watch:
	sudo dune build --watch
run:
	sudo dune exe ./src/main.exe
show: 
	sudo dune exec -- ocaml-print-intf $(P)

