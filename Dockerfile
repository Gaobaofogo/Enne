FROM haskell:latest

WORKDIR /app

COPY . .

RUN cabal update

ENTRYPOINT ["tail", "-f", "/dev/null"]