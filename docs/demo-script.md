# Demo Script

Use this short walkthrough for recruiters, interviewers, or reviewers. The goal is to show platform engineering judgment around a deliberately small application.

## 1. Position the project

- Explain that TODO CRUD is intentionally simple.
- Call out that the portfolio value is in local reproducibility, Kubernetes shape, CI validation, observability, security, and AKS/GitOps readiness.
- Open `docs/platform-milestones.md` and show the phased roadmap.

## 2. Run locally

```sh
cp .env.example .env
docker compose up --build -d
docker compose ps
```

Open the frontend at <http://localhost:3000> and the gateway at <http://localhost/todo>.

## 3. Show service boundaries

- Open `docs/architecture.md` and walk through the gateway-to-service request flow.
- Show `api-gateway.conf` to explain method-based routing.
- Mention that the polyglot services make CI and container validation more realistic than a single-stack demo.

## 4. Show optional developer tooling

```sh
docker compose --profile tools up -d
```

Open phpMyAdmin at <http://localhost:8080> only as a local development aid.

## 5. Close with the roadmap

- Milestone 1: harden local runtime, containers, Kubernetes manifests, and CI.
- Milestone 2: AKS, ACR, Terraform, and Flux.
- Milestone 3: dashboards, alerts, SLOs, and runbooks.
- Milestone 4: secrets, workload identity, network policy, admission policy, and supply-chain controls.

## Next recommended step

Open a pull request that shows the Milestone 1 infrastructure gates: container builds, Dockerfile linting, and rendered Kubernetes validation.
