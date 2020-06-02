# STAGE 1: Build
FROM golang:1.14 AS build

WORKDIR /go/src/app
COPY . .

RUN go build -o stage

# STAGE 2: Deployment
FROM alpine

USER nobody:nobody
COPY --from=build /go/src/app/stage /stage
CMD ["/stage"]
