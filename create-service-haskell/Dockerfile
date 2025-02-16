FROM haskell:9.4

WORKDIR /app

RUN cabal update

# Add just the .cabal file to capture dependencies
# COPY ./create-service-haskell.cabal create-service-haskell.cabal

COPY . .

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
# RUN cabal build --only-dependencies -j4
RUN cabal install --only-dependencies

# Add and Install Application Code
RUN cabal build
# RUN cabal install

EXPOSE 3000

# Run the application
CMD ["cabal", "run"]