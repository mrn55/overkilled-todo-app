FROM haskell:9.4.8 AS build

WORKDIR /app

RUN cabal update

COPY create-service-haskell.cabal LICENSE CHANGELOG.md ./
COPY app ./app

RUN cabal install --only-dependencies \
    && cabal build

FROM debian:12.8-slim

LABEL org.opencontainers.image.title="Overkilled Todo Create Service" \
      org.opencontainers.image.description="Haskell CRUD create service for the Overkill(ed) Todo App" \
      org.opencontainers.image.source="https://github.com/mrn55/overkilled-todo-app" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.licenses="MIT"

# https://packages.debian.org/search?mode=path&suite=sid&section=all&arch=any&searchon=contents&keywords=libmariadb.so.3
# https://packages.debian.org/search?searchon=contents&keywords=libpcre.so.3&mode=path&suite=unstable&arch=any
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libmariadb3 \
    libpcre3 \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --system --gid 10001 app \
    && useradd --system --uid 10001 --gid app --home-dir /app --shell /usr/sbin/nologin app

WORKDIR /app

COPY --from=build --chown=app:app /app/dist-newstyle/build/x86_64-linux/ghc-*/create-service-haskell-*/x/create-service-haskell/build/create-service-haskell .

USER app

CMD ["./create-service-haskell"]
