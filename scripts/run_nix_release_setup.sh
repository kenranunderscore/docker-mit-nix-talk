#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

nix build .#release
docker load < result

docker run --rm --name bob -t -d --network host both-backends
sleep 5

echo
docker run --rm -it --network host python-demo

echo
docker logs bob

echo
echo "Stopping service..."
echo
docker stop $(docker ps -a -q) >/dev/null
