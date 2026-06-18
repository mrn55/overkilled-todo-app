# Architecture

## Intent

The application is intentionally simple: users create, read, update, and delete TODO items. The architecture is intentionally overbuilt to demonstrate platform engineering around a small business capability.

## Runtime topology

```text
Browser
  |
  | HTTP
  v
todo-frontend (React served by NGINX)
  |
  | /todo API calls
  v
api-gateway (NGINX)
  |-- POST /todo ----> create-service (Haskell/Scotty)
  |-- GET /todo -----> read-service (Go/Gin)
  |-- PUT /todo/:id -> update-service (Rust/Axum)
  `-- DELETE /todo/:id -> delete-service (Erlang/Cowboy)
                         |
                         v
                       MariaDB
```

## Request flow

1. A user opens the frontend in a browser.
2. The frontend sends CRUD requests to the configured API base URL.
3. The NGINX gateway accepts API traffic and routes by HTTP method to the narrow backend service responsible for that operation.
4. Backend services use shared database credentials from local environment variables or Kubernetes secrets/configuration.
5. MariaDB stores TODO rows initialized by `init.sql` for local development.

## Platform components

- **Docker Compose** provides deterministic local startup and optional developer tooling profiles.
- **Kubernetes manifests** are the foundation for local Minikube and future AKS overlays.
- **Documentation** captures runbooks, architecture, demo flow, and decision records.
- **Planned CI** will validate every language stack, Dockerfiles, filesystem security findings, and rendered Kubernetes manifests.
- **Planned GitOps and Azure IaC** will promote the same simple app to AKS without changing the business behavior.

## Current milestone alignment

This document supports Milestone 1, Foundation polish, by making the service topology and platform intent explicit before deeper Kubernetes, CI, and container-hardening work continues.
