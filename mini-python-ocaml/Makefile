
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s && ./a.out

parse: minipython.exe
	./minipython.exe --parse-only test.py

type: minipython.exe
	./minipython.exe --type-only test.py

minipython.exe:
	dune build minipython.exe

test_typing: minipython.exe
	@echo "Running tests..."
	cd tests-mini-python && ./test -2 ../minipython.exe
	cd ..

test_compile: minipython.exe
	@echo "Running tests..."
	cd tests-mini-python && ./test -3 ../minipython.exe
	cd ..

format:
	opam exec -- dune fmt > /dev/null 2>&1 || true

clean:
	dune clean

.PHONY: all clean minipython.exe



