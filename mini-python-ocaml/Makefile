
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s && ./a.out

tests: minipython.exe
	bash run-tests "dune exec ./minipython.exe"

parse: minipython.exe
	./minipython.exe --parse-only test.py

type: minipython.exe
	./minipython.exe --type-only test.py

minipython.exe:
	dune build minipython.exe

clean:
	dune clean

.PHONY: all clean minipython.exe



