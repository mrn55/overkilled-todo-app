FROM rust:latest

WORKDIR /app

COPY Cargo.toml Cargo.lock ./

COPY . .

RUN cargo fetch

RUN cargo build --release

CMD ["./target/release/update-service-rust"]
