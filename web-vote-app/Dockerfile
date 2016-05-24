FROM haskell

WORKDIR /opt/server

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./votacion.cabal /opt/server/votacion.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/server
RUN cabal build


ENV PORT 80
EXPOSE 80

CMD ["cabal", "run"]
