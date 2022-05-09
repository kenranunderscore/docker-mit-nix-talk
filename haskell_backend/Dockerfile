FROM ubuntu

COPY . .
RUN apt-get update
RUN apt-get install -y cabal-install zlib1g-dev
RUN cabal update
RUN cabal build
CMD cabal run
