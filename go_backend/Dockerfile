# Warning: do not use this in production!

FROM golang

WORKDIR /build

COPY . .
RUN go install
CMD go run server
