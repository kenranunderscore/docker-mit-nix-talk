# Warning: do not use this in production!

FROM golang

WORKDIR /build

COPY . .
RUN apt-get update
RUN apt-get install -y make
RUN make
CMD go run server
