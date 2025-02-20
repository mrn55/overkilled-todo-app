FROM golang:latest AS build

WORKDIR /app

COPY go.mod go.sum ./
RUN go mod download

COPY . .

# RUN go build -o read-service-go
RUN CGO_ENABLED=0 GOOS=linux go build -o read-service-go .

FROM alpine:3.21

WORKDIR /app

COPY --from=build /app/read-service-go .

ENTRYPOINT ["/app/read-service-go"]