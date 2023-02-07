#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

docker build -t go-backend-0 ../go_backend
docker build -t haskell-backend-0 ../haskell_backend
docker build -t python-client-0 ../python_client

# Backends
docker run --rm --name gob -t -d --network host go-backend-0
docker run --rm --name hab -t -d --network host haskell-backend-0
sleep 5 # you wouldn't do this in production, of course...

echo
echo "=================================="
echo "        PYTHON CLIENT LOGS        "
echo "=================================="
echo
docker run --rm -it --network host python-client-0

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
