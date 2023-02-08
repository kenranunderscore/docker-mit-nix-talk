#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

nix build .#haskellBackend
docker load < result

nix build .#goBackend
docker load < result

nix build .#pythonClient
docker load < result

# Backends
docker run --rm --name gob -t -d --network host go-backend
docker run --rm --name hab -t -d --network host haskell-backend
sleep 5

echo
docker run --rm -it --network host python-demo

echo
docker logs gob

echo
docker logs hab

echo
echo "Stopping services..."
echo
docker stop $(docker ps -a -q) >/dev/null
