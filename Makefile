build:
	@opam exec -- dune build

run:
	@opam exec -- dune exec adventofcode2025

repl:
	@opam exec -- dune utop .