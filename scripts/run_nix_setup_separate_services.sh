#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

nix-build ../nix/haskell-docker-image.nix
docker load < result
nix-build ../nix/python-client-docker-image.nix
docker load < result
nix-build ../nix/go-docker-image.nix
docker load < result

# Backends
docker run --rm --name gob -t -d --network host go-backend
docker run --rm --name hab -t -d --network host haskell-backend
sleep 5

echo
echo "=================================="
echo "        PYTHON CLIENT LOGS        "
echo "=================================="
echo
docker run --rm -it --network host python-demo

echo "=================================="
echo "         GO BACKEND LOGS          "
echo "=================================="
echo
docker logs gob

echo
echo "=================================="
echo "       HASKELL BACKEND LOGS       "
echo "=================================="
echo
docker logs hab

echo
echo "Stopping services..."
echo
docker stop $(docker ps -a -q) >/dev/null
