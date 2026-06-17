

run: build
	./_build/default/main.exe

build:
	dune build

run: build
	./_build/default/main.exe

tests:
	dune test

# unfortunately, dune clean alone leaves merlin spore in ./_build
clean:
	rm -rf ./_build && dune clean 
