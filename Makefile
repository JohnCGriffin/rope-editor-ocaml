

build:
	dune build

tests:
	dune test

# unfortunately, dune clean alone leaves merlin spore in ./_build
clean:
	rm -rf ./_build && dune clean 
