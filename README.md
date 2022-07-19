# Enne

<p align="center">
  <img src="https://github.com/Gaobaofogo/Enne/blob/main/docs/img/enne.png" width="700">
</p>

## Buildando o projeto


```bash
docker build --pull --rm -f "Dockerfile" -t enne:latest "."
docker run -d enne:latest
docker exec -it $(docker ps -a | grep enne | awk '{print $1}') /bin/bash
```

Dentro do container, para compilar e rodar o projeto a partir da raiz:

```
make
./main problemas/<nome_do_arquivo>.enne
```

Caso não esteja dentro do container, é recomendado usar os passos de instalação a seguir:

1. Seguir a instalação desse site https://www.haskell.org/ghcup/. Aceite todos os pacotes que ele pedir para instalar. Entre eles estão o próprio haskell, pacotes que o vscode precisa para analisar corretamente o código e o cabal que ainda usaremos para instalar os pacotes.
2. Rode os seguintes comandos:
```
cabal update
cabal install parsec alex --lib
```

## Usando o alex

Para executar o analisador léxico(alex) com a nossa linguagem você deve compilar o arquivo lexer.x e para testar pode entrar no modo interativo. A função de execução para ler os tokens é a função <i>getTokens</i> que recebe como parâmetro o caminho do arquivo. É importante frisar que a saída do alex será utilizado no parsec dentro do arquivo main. Na linha de comando, fica assim para testar o alex:

```bash
$ alex lexer.x
```

