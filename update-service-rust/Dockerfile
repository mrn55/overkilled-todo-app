FROM rust:1.84.1-bookworm AS builder
WORKDIR /app
COPY . .
RUN cargo install --locked --path .

FROM debian:12.8-slim

LABEL org.opencontainers.image.title="Overkilled Todo Update Service" \
      org.opencontainers.image.description="Rust CRUD update service for the Overkill(ed) Todo App" \
      org.opencontainers.image.source="https://github.com/mrn55/overkilled-todo-app" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.licenses="MIT"

RUN groupadd --system --gid 10001 app \
    && useradd --system --uid 10001 --gid app --home-dir /app --shell /usr/sbin/nologin app

COPY --from=builder --chown=app:app /usr/local/cargo/bin/update-service-rust /usr/local/bin/update-service-rust

USER app

CMD ["update-service-rust"]
