FROM golang:1.24.0-alpine3.21 AS build

WORKDIR /app

COPY go.mod go.sum ./
RUN go mod download

COPY . .

RUN CGO_ENABLED=0 GOOS=linux go build -trimpath -ldflags="-s -w" -o read-service-go .

FROM alpine:3.21.2

LABEL org.opencontainers.image.title="Overkilled Todo Read Service" \
      org.opencontainers.image.description="Go CRUD read service for the Overkill(ed) Todo App" \
      org.opencontainers.image.source="https://github.com/mrn55/overkilled-todo-app" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.licenses="MIT"

RUN addgroup -S -g 10001 app && adduser -S -u 10001 -G app app

WORKDIR /app

COPY --from=build --chown=app:app /app/read-service-go .

USER app

ENTRYPOINT ["/app/read-service-go"]
