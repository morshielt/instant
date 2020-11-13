all:
	ghc -isrc/parser:src/compilers src/Main.hs -o compiler
