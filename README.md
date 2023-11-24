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

## Enne Syntax Highlights 

Na pasta `~/.vscode/extensions/` clone o repositório abaixo:

```bash
git clone git@github.com:nataly-enne/enne-syntax-highlight.git
```

Em seguida, feche e abra o VScode e o plugin irá aplicar automaticamente.

## Colaboradores
<table>
  <tr>
    <td align="center">
      <a href="https://github.com/nataly-enne">
        <img src="https://avatars3.githubusercontent.com/u/26802307?s=400&v=4" width="100px;" alt="Nátaly Enne"/>
        <br />
        <sub><b>Nátaly Enne</b></sub>
      </a><br />
      <a href="https://github.com/Gaobaofogo/Enne/commits?author=nataly-enne" title="Code">💻</a>
    </td>
    <td align="center">
      <a href="https://github.com/Gaobaofogo">
        <img src="https://avatars.githubusercontent.com/u/22860539?v=4" width="100px;" alt="Gabriel Augusto"/>
        <br />
        <sub><b>Gabriel Augusto</b></sub>
      </a><br />
      <a href="https://github.com/Gaobaofogo/Enne/commits?author=edusrmt" title="Code">💻</a>
    </td>
    <td align="center">
      <a href="https://github.com/isaacrpl7">
        <img src="https://avatars.githubusercontent.com/u/50757880?v=4" width="100px;" alt="Isaac Lima"/>
        <br />
        <sub><b>Isaac Lima</b></sub>
      </a><br />
      <a href="https://github.com/Gaobaofogo/Enne/commits?author=isaacrpl7" title="Code">💻</a>
    </td>
    <td align="center">
      <a href="https://github.com/lluckymou">
        <img src="https://avatars.githubusercontent.com/u/19612449?v=4" width="100px;" alt="Lucas Nascimento"/>
        <br />
        <sub><b>Lucas Nascimento</b></sub>
      </a><br />
      <a href="https://github.com/Gaobaofogo/Enne/commits?author=lluckymou" title="Code">💻</a>
    </td>
  </tr>
</table>
