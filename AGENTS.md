# AGENTS.md

## Project intent

This repository is a platform engineering portfolio project built around an intentionally simple TODO CRUD application. Do not make the business application more complex unless explicitly asked. The project should showcase production-grade DevOps, SRE, Azure, AKS, GitOps, observability, and security practices around a small polyglot app.

## Working priorities

When choosing what to implement, prefer changes that create strong platform-engineering signal:

1. Repeatable local development and CI validation.
2. Container hardening and supply-chain hygiene.
3. Kubernetes/Kustomize structure that can promote from local to AKS.
4. Azure infrastructure as code for AKS, ACR, Key Vault, identities, networking, and monitoring.
5. GitOps reconciliation with clear environment overlays.
6. Observability with health checks, metrics, dashboards, alerts, SLOs, and runbooks.
7. Enterprise security with external secrets, workload identity, network policy, admission policy, mTLS, SBOMs, and image scanning/signing.

## Scope guardrails

- Keep TODO CRUD behavior simple.
- Do not add extra product features just to make the app more interesting.
- Prefer practical repo changes over abstract recommendations.
- Keep work realistic for one person and split changes into reviewable PRs.
- Favor boring, standard, production-shaped patterns over cleverness.
- Document demo moments and acceptance criteria for platform changes.

## Repository roadmap

Use `docs/platform-milestones.md` as the primary planning document. New work should generally map to one of these phases:

1. Foundation polish.
2. AKS and GitOps.
3. Observability.
4. Enterprise security and policy polish.

If a task does not fit these phases, update the roadmap or explain why it is intentionally out of sequence.

## File and structure conventions

- Keep platform documentation under `docs/`.
- Keep local Kubernetes and app manifests under `k8s/`, preferably organized with Kustomize `base` and `overlays`.
- Keep Azure infrastructure-as-code under `infra/terraform/` unless a different tool is explicitly chosen.
- Keep GitOps cluster state under `clusters/` and reusable GitOps app/infrastructure definitions under `gitops/`.
- Keep observability assets under `observability/`.
- Keep policy-as-code under `policy/`.
- Keep security documentation under `security/`.

## Validation expectations

For code or config changes, run the most relevant checks available in the repo. Prefer adding automated checks when introducing new toolchains. Useful checks may include:

- Frontend: `npm ci`, `npm run lint`, `npm run build` from `todo-frontend/`.
- Go: `go test ./...` and `go vet ./...` from `read-service-go/`.
- Rust: `cargo fmt --check`, `cargo clippy`, and `cargo test` from `update-service-rust/`.
- Haskell: `cabal build` from `create-service-haskell/`.
- Erlang: `rebar3 compile` from `delete-service-erlang/`.
- Kubernetes: `kustomize build` and schema validation when Kustomize is introduced.
- Terraform: `terraform fmt -check`, `terraform validate`, and plan for affected environments when Terraform is introduced.
- Docs-only changes: at minimum run `git diff --check`.

## Communication expectations

When summarizing changes, cite the files modified and call out which milestone the work supports. For planning or documentation work, include the next recommended implementation step so the project keeps moving.
