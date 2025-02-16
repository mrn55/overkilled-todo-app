# Todo Update Service in Rust

Step 1:
Initialize this.
```
# this did not work, but getting in the container as below did:
# PS> docker run --rm -v ${PWD}:/app rust:latest cargo init --bin
PS C:\projects\GitHub\overly-complex-todo-app\update-service-rust> docker run --rm -it -v ${PWD}:/app rust:latest bash
root@c3e679fd85bc:/# cd /app
root@c3e679fd85bc:/app# dir
Dockerfile  README.md
root@c3e679fd85bc:/app# cargo init --name update-service-rust --bin
    Creating binary (application) package
note: see more `Cargo.toml` keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html
```

Step 2:
Add some deps
```
docker run --rm -v ${PWD}:/app -w /app rust:latest cargo add axum tokio sqlx mysql
```
