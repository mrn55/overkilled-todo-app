FROM rust:latest AS builder
WORKDIR /app
COPY . .
RUN cargo install --path .

FROM debian:bookworm-slim
COPY --from=builder /usr/local/cargo/bin/update-service-rust /usr/local/bin/update-service-rust
CMD ["update-service-rust"]