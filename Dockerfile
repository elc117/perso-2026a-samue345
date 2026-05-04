FROM haskell:9.8

RUN apt-get update && apt-get install -y \
    sqlite3 \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*


WORKDIR /app

COPY laundry-api.cabal .

RUN cabal update && cabal build --only-dependencies

COPY . .

RUN cabal build

EXPOSE 8080

CMD ["cabal", "run",  "mkdir -p data && touch data/laundry.sqlite3 && cabal run laundry-api"]
