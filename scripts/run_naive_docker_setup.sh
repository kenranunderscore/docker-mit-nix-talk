#!/usr/bin/env bash

cd "$(dirname $0)"

docker build -t go-backend-0 ../go_backend
docker build -t haskell-backend-0 ../haskell_backend
docker build -t python-client-0 ../python_client

docker run --rm -t -d --network host go-backend-0
docker run --rm -t -d --network host haskell-backend-0
sleep 2
docker run --rm -it --network host python-client-0

docker stop $(docker ps -a -q)
