# Enne

## Buildando o projeto

Para criar e entrar no container:

```bash
$ docker build --pull --rm -f "Dockerfile" -t enne:latest "."
$ docker run -d enne:latest
$ docker exec -it $(docker ps -a | grep enne | awk '{print $1}') /bin/bash
```

Dentro do container, para compilar e rodar o projeto a partir da raiz:

```
$ make
$ ./main <nome_do_arquivo>.enne
```

Caso não esteja dentro do container, é recomendado usar os passos de instalação a seguir:

1. Seguir a instalação desse site https://www.haskell.org/ghcup/. Aceite todos os pacotes que ele pedir para instalar. Entre eles estão o próprio haskell, pacotes que o vscode precisa para analisar corretamente o código e o cabal que ainda usaremos para instalar os pacotes.
2. Rode os seguintes comandos:
```
$ cabal update
$ cabal install parsec alex --lib
```

## Usando o alex

Para executar o analisador léxico(alex) com a nossa linguagem você deve compilar o arquivo lexer.x e para testar pode entrar no modo interativo. A função de execução para ler os tokens é a função <i>getTokens</i> que recebe como parâmetro o caminho do arquivo. É importante frisar que a saída do alex será utilizado no parsec dentro do arquivo main. Na linha de comando, fica assim para testar o alex:

```bash
$ alex lexer.x
$ ghci lexer.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /home/gabriel/.ghc/x86_64-linux-8.10.7/environments/default
[1 of 1] Compiling Lexer            ( lexer.hs, interpreted )
Ok, one module loaded.
*Lexer> getTokens "programa.enne"
[Type "int",Id "x",Assign,Int 37,SemiColon]
```

Onde no arquivo "programa.enne" está escrito:

```
int x = 37;
```

## Usando o Parsec

O parsec vai receber o que o alex vai ler e processar. Essa parte ainda não está sendo feita.
