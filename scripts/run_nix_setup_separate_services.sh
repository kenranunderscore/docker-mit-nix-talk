#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

nix-build ../nix/haskell-docker-image.nix
docker load < result
nix-build ../nix/python-client-docker-image.nix
docker load < result
nix-build ../nix/go-docker-image.nix
docker load < result

docker run --rm --name gob -t -d --network host go-backend
docker run --rm --name hab -t -d --network host haskell-backend
sleep 2
docker run --rm -it --network host python-demo

echo "=================================="
echo "           SERVICE LOGS           "
echo "=================================="

docker logs gob
echo ""
docker logs hab

docker stop "$(docker ps -a -q)"
