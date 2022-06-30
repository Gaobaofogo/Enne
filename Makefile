all:
	alex ./app/lexer.x
	ghc ./app/Main.hs ./app/lexer.hs  ./app/expression.hs ./app/token.hs
	rm -rf ./app/*.hi ./app/*.o