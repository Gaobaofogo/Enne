# Enne

Para compilar o projeto e entrar no container:

```bash
$ docker build --pull --rm -f "Dockerfile" -t enne:latest "."
$ docker run -d enne:latest
$ docker exec -it $(docker ps -a | grep enne | awk '{print $1}') /bin/bash
# 
```

Dentro do container, para compilar e rodar o projeto:

```
$ cabal build
$ cabal run enne
```
