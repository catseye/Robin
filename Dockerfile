from haskell:9.4.5-slim-buster

WORKDIR /opt/robin

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./Robin.cabal /opt/robin/Robin.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/robin
RUN cabal install

CMD ["robin"]

