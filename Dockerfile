FROM haskell:latest

WORKDIR /app

RUN cabal update
RUN cabal install matrix parsec alex --lib

COPY . .

ENTRYPOINT ["tail", "-f", "/dev/null"]