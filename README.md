# Overkill(ed) Todo App

A deliberately simple TODO CRUD application wrapped in production-shaped platform engineering practices. The business domain is intentionally small; the portfolio signal comes from repeatable local development, hardened containers, Kubernetes/Kustomize foundations, CI validation, observability, security, and Azure/AKS GitOps evolution.

## Why this project exists

Most portfolio CRUD apps make the product more complex. This repository does the opposite: it keeps TODO behavior boring so platform work is easy to review, demo, and operate. The application is polyglot on purpose so CI, containers, and Kubernetes validation have to handle realistic heterogeneity.

## Service topology

| Component | Stack | Responsibility |
| --- | --- | --- |
| `todo-frontend` | Vite, React, TypeScript, NGINX | Browser UI for listing and changing todos. |
| `api-gateway` | NGINX | Single HTTP entry point that routes CRUD methods to backend services. |
| `create-service` | Haskell, Scotty | Creates todos in MariaDB. |
| `read-service` | Go, Gin | Reads todos from MariaDB. |
| `update-service` | Rust, Axum | Updates todos in MariaDB. |
| `delete-service` | Erlang, Cowboy | Deletes todos from MariaDB. |
| `db` | MariaDB | Local relational datastore. |
| `phpmyadmin` | phpMyAdmin | Optional local-only database inspection tool. |

See [`docs/architecture.md`](docs/architecture.md) for request flow and platform components.

## Quickstart: Docker Compose

Prerequisites:

- Docker Engine with Docker Compose v2.
- Ports `80` and `3000` available for the gateway and frontend.

```sh
git clone https://github.com/mrn55/overkilled-todo-app.git
cd overkilled-todo-app
cp .env.example .env
docker compose up --build -d
```

Open:

- Frontend: <http://localhost:3000>
- API gateway: <http://localhost/todo>

Optional local tools:

```sh
# Add phpMyAdmin on http://localhost:8080
docker compose --profile tools up --build -d

# Expose backend service ports directly for debugging
docker compose --profile backend-ports up --build -d
```

Useful checks:

```sh
docker compose ps
docker compose logs -f api-gateway
docker compose down --remove-orphans
```

## Quickstart: local Kubernetes

Use [`MINIKUBE.md`](MINIKUBE.md) for the current local Kubernetes runbook. Milestone 1 is moving flat manifests toward `k8s/base` and `k8s/overlays/local` so the same app shape can later promote to AKS.

## Platform roadmap

The project roadmap lives in [`docs/platform-milestones.md`](docs/platform-milestones.md):

1. **Foundation polish**: deterministic local runtime, container hardening, Kubernetes validation, CI quality gates.
2. **AKS and GitOps**: Terraform-managed Azure infrastructure, ACR image release, Flux reconciliation.
3. **Observability**: metrics, dashboards, alerts, SLOs, runbooks, and load demos.
4. **Enterprise security and policy polish**: secret management, workload identity, network policy, admission policy, and supply-chain controls.

## Demo scripts

- [`docs/demo-script.md`](docs/demo-script.md): short recruiter/interviewer walkthrough.
- [`MINIKUBE.md`](MINIKUBE.md): local cluster runbook.

## Development guardrails

- Do not make TODO CRUD behavior more complex unless explicitly requested.
- Prefer platform changes that are reviewable in small pull requests.
- Keep local-only conveniences out of production-shaped overlays.
- Document acceptance criteria and demo moments for platform work.
