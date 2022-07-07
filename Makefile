all:
	alex ./app/lexer.x
	ghc ./app/Main.hs ./app/lexer.hs ./app/memory.hs  ./app/expression.hs ./app/statements.hs ./app/token.hs -o main
	rm -rf ./app/*.hi ./app/*.o