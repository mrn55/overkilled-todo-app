FROM haskell:9.4 AS build

WORKDIR /app

RUN cabal update

COPY . .

RUN cabal install --only-dependencies

RUN cabal build

FROM debian:bookworm-slim

# https://packages.debian.org/search?mode=path&suite=sid&section=all&arch=any&searchon=contents&keywords=libmariadb.so.3
# https://packages.debian.org/search?searchon=contents&keywords=libpcre.so.3&mode=path&suite=unstable&arch=any
RUN apt-get update && apt-get install -y \
    libmariadb3 \
    libpcre3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/dist-newstyle/build/x86_64-linux/ghc-*/create-service-haskell-*/x/create-service-haskell/build/create-service-haskell .

CMD ["./create-service-haskell"]