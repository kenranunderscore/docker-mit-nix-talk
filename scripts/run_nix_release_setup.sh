#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

nix-build ../nix/docker-release.nix
docker load < result

docker run --rm --name bob -t -d --network host both-backends
sleep 2
docker run --rm -it --network host python-demo

echo "=================================="
echo "           SERVICE LOGS           "
echo "=================================="
echo

docker logs bob

docker stop $(docker ps -a -q) >/dev/null
