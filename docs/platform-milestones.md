# Platform Engineering Build Milestones

This roadmap turns the Overkill(ed) Todo App into a production-grade Azure/AKS platform engineering portfolio project. The business app stays intentionally simple; the platform around it becomes the showcase.

## Guiding principles

- Keep TODO CRUD simple and avoid adding domain complexity.
- Prioritize repo changes that are demoable, reviewable, and realistic for one person.
- Build in small pull requests with clear acceptance criteria.
- Prefer boring, production-shaped platform patterns over novelty.
- Make every phase produce a visible demo moment.

## Milestone 1: Foundation polish

### Objective

Make the repository credible before adding cloud complexity: deterministic local development, hardened containers, validated Kubernetes manifests, and CI quality gates.

### Tasks

#### Documentation and repo hygiene

- [x] Rewrite `README.md` as a portfolio landing page with architecture, quickstarts, platform roadmap, and demo script links.
- [x] Update `MINIKUBE.md` into a reliable local Kubernetes runbook.
- [x] Add `.env.example` and align README setup instructions with it.
- [x] Add or update `.gitignore` for `.env`, local kubeconfig files, generated reports, screenshots, and tool caches.
- [x] Add `docs/architecture.md` describing service topology, request flow, and platform components.
- [x] Add `docs/demo-script.md` with a short recruiter/interviewer walkthrough.
- [x] Add `docs/decisions/0001-keep-business-app-simple.md` explaining why platform depth is the project focus.

#### Local runtime and gateway polish

- [x] Add Docker Compose profiles for default app runtime, development-only phpMyAdmin, and optional direct backend port exposure.
- [x] Add Docker Compose healthchecks for MariaDB, frontend, gateway, and backend services.
- [x] Pin Docker Compose runtime images instead of using floating `latest` tags.
- [x] Add Docker networks so only intended services are reachable from the host.
- [x] Replace unsafe NGINX method-routing patterns in `api-gateway.conf` with production-shaped routing.
- [x] Add `/healthz` to the gateway and frontend NGINX config.
- [x] Make frontend API base URL runtime-configurable instead of hardcoding `http://localhost/todo`.

#### Container hardening

- [ ] Pin base image versions in all service Dockerfiles.
- [ ] Add OCI image labels to all application images.
- [ ] Add non-root runtime users where feasible.
- [ ] Add `.dockerignore` files for each build context.
- [ ] Change frontend build from `npm install` to `npm ci`.
- [ ] Replace the Erlang production container command with a production-oriented release/start command.

#### Kubernetes foundation

- [ ] Reorganize flat `k8s/*.yaml` manifests into `k8s/base` and `k8s/overlays/local`.
- [ ] Add Kustomize `kustomization.yaml` files for base and local overlays.
- [ ] Add a dedicated namespace manifest.
- [ ] Add labels and annotations consistently across workloads and services.
- [ ] Add resource requests and limits to every workload.
- [ ] Add readiness and liveness probes to every workload.
- [ ] Add basic pod security contexts.
- [ ] Replace local-only image settings such as `imagePullPolicy: Never` in non-local overlays.

#### CI quality gates

- [ ] Add `.github/workflows/ci.yaml` for language build and lint checks.
- [ ] Add `.github/workflows/k8s-validate.yaml` for `kustomize build` and Kubernetes schema validation.
- [ ] Add `.github/workflows/container-build.yaml` for image builds and Dockerfile linting.
- [ ] Add Trivy filesystem scanning for dependency and secret findings.
- [ ] Add a status badge section to the README after workflows exist.

### Acceptance criteria

- [ ] A reviewer can run the app locally from documented instructions.
- [ ] CI validates frontend, Go, Rust, Haskell, Erlang, Dockerfiles, and Kubernetes manifests.
- [ ] Kustomize renders the local environment successfully.
- [ ] Every workload has resources, probes, labels, and a baseline security context.
- [ ] README clearly states that the app is intentionally simple and the platform is the focus.

### Demo moment

Show a clean pull request where CI builds every language, validates Kubernetes, scans the repo, and renders a local Kustomize deployment for the same simple TODO app.

### Complexity traps to avoid

- Do not add TODO business features.
- Do not introduce AKS, Flux, Istio, Prometheus, and Key Vault in the same milestone.
- Do not over-abstract Kubernetes before the base/local overlay works.
- Do not make health probes depend on database writes.

## Milestone 2: AKS and GitOps

### Objective

Provision Azure infrastructure and deploy the application through GitOps so the repo demonstrates an end-to-end AKS platform workflow.

### Tasks

#### Infrastructure as code

- [ ] Add `infra/terraform/versions.tf`, `providers.tf`, `variables.tf`, `outputs.tf`, and `README.md`.
- [ ] Add Terraform for resource group, virtual network, AKS, ACR, Log Analytics, Key Vault, and managed identities.
- [ ] Add `infra/terraform/environments/dev.tfvars` and `prod.tfvars.example`.
- [ ] Add Terraform validation and plan workflow.
- [ ] Document manual approval expectations for Terraform apply.

#### Azure container registry and image release

- [ ] Add GitHub Actions workflow to build all service images.
- [ ] Push versioned images to Azure Container Registry.
- [ ] Generate SBOM artifacts for each image.
- [ ] Run image vulnerability scanning before publishing or promotion.
- [ ] Update Kustomize image tags automatically after successful release.

#### GitOps structure

- [ ] Add `clusters/aks-dev` as the Flux bootstrap target.
- [ ] Add `gitops/apps/todo` to point Flux at the app overlay.
- [ ] Add `gitops/infrastructure` for ingress-nginx, cert-manager, external-secrets, policy, and monitoring placeholders.
- [ ] Add reconciliation documentation and common Flux commands.
- [ ] Keep local and AKS deployment paths separate.

#### AKS overlays

- [ ] Add `k8s/overlays/aks-dev` with AKS-specific ingress, images, resources, and replica settings.
- [ ] Add `k8s/overlays/aks-prod` as a realistic production-shaped example without requiring immediate deployment.
- [ ] Replace local hostnames with overlay-specific hostnames.
- [ ] Add HorizontalPodAutoscaler resources where service behavior supports it.
- [ ] Decide whether MariaDB remains in-cluster for demo cost control and document the managed Azure database upgrade path.

### Acceptance criteria

- [ ] `terraform validate` and dev `terraform plan` pass.
- [ ] AKS and ACR can be created from repo instructions.
- [ ] Images are built, scanned, tagged, and pushed to ACR.
- [ ] Flux reconciles the app from the GitOps folder into AKS.
- [ ] The AKS overlay exposes frontend and API through ingress.
- [ ] No production-like overlay depends on local image names or `imagePullPolicy: Never`.

### Demo moment

Show a commit that builds images, pushes them to ACR, updates GitOps desired state, and is reconciled by Flux into AKS.

### Complexity traps to avoid

- Do not build a reusable Terraform module framework before the single environment works.
- Do not require paid DNS or private AKS for the first successful demo.
- Do not hide operational flow behind too much automation; reviewers should see the pieces.
- Do not present in-cluster MariaDB as enterprise production without documenting the managed database path.

## Milestone 3: Observability

### Objective

Make the platform operable with metrics, logs, dashboards, alerts, SLOs, runbooks, and failure demos.

### Tasks

#### Health and metrics

- [ ] Add `/healthz` and `/readyz` endpoints to each backend service.
- [ ] Add `/metrics` to at least the gateway and one backend first, then expand incrementally.
- [ ] Add Kubernetes probes that use health and readiness endpoints.
- [ ] Add request IDs and structured logs at the gateway.
- [ ] Standardize log fields across services where practical.

#### Monitoring stack

- [ ] Move existing Prometheus experiments into `k8s/base/observability` or replace with a GitOps-managed monitoring stack.
- [ ] Replace the current generic ServiceMonitor with app-specific ServiceMonitors.
- [ ] Add PrometheusRule alerts for high 5xx rate, high latency, crash loops, low replica availability, and database unavailability.
- [ ] Avoid exposing Prometheus through NodePort in AKS overlays.
- [ ] Document Azure Managed Prometheus and Grafana options.

#### Dashboards, SLOs, and runbooks

- [ ] Add `observability/dashboards/todo-overview.json`.
- [ ] Add `observability/dashboards/service-golden-signals.json`.
- [ ] Add `observability/slos/todo-api-slo.md`.
- [ ] Add `observability/runbooks/high-error-rate.md`.
- [ ] Add `observability/runbooks/database-unavailable.md`.
- [ ] Add `observability/runbooks/pod-crashloop.md`.
- [ ] Add `observability/load/k6-smoke.js` and `k6-load.js`.

#### Optional tracing

- [ ] Add OpenTelemetry Collector manifests.
- [ ] Instrument one request path end-to-end before attempting every service.
- [ ] Add a trace demo section to `observability/README.md`.

### Acceptance criteria

- [ ] Dashboards show request rate, error rate, latency, pod restarts, CPU, memory, and ingress status.
- [ ] Alerts are tied to documented runbooks.
- [ ] At least one SLO has an error budget explanation.
- [ ] k6 can generate traffic for a live dashboard demo.
- [ ] A failure scenario can be triggered and recovered using a runbook.

### Demo moment

Run a k6 load test, watch golden signals in Grafana, intentionally break a dependency, observe alerts, follow the runbook, and restore service.

### Complexity traps to avoid

- Do not instrument every language perfectly before dashboards work.
- Do not add tracing before useful metrics and logs exist.
- Do not create too many dashboards; two or three strong dashboards are better than noisy sprawl.
- Do not make alerts that are disconnected from user impact or remediation.

## Milestone 4: Enterprise security and policy polish

### Objective

Add security controls that show enterprise platform maturity: secret management, workload identity, network isolation, admission policy, supply-chain integrity, and service mesh authorization.

### Tasks

#### Secrets and identity

- [ ] Remove committed base64 Kubernetes Secrets from application manifests.
- [ ] Add Azure Key Vault-backed External Secrets or SOPS-encrypted GitOps secrets.
- [ ] Configure AKS workload identity for secret access.
- [ ] Document local-development secret handling separately from AKS secret handling.
- [ ] Add secret scanning to CI if not already present.

#### Pod and network hardening

- [ ] Add default-deny NetworkPolicy per namespace.
- [ ] Allow frontend to gateway traffic only where required.
- [ ] Allow gateway to CRUD service traffic only where required.
- [ ] Allow CRUD services to MariaDB traffic only where required.
- [ ] Add service accounts for workloads.
- [ ] Enforce `runAsNonRoot`, `allowPrivilegeEscalation: false`, dropped capabilities, and read-only root filesystems where feasible.

#### Policy as code

- [ ] Add Kyverno or Conftest policies under `policy/`.
- [ ] Block `latest` image tags in production overlays.
- [ ] Require resource requests and limits.
- [ ] Require readiness and liveness probes.
- [ ] Block privileged containers.
- [ ] Restrict NodePort usage in production overlays.
- [ ] Add policy validation to CI against rendered manifests.

#### Supply chain and service mesh

- [ ] Add Cosign image signing or keyless signing documentation.
- [ ] Publish SBOMs as CI artifacts.
- [ ] Add provenance generation where feasible.
- [ ] Expand Istio strict mTLS with AuthorizationPolicy.
- [ ] Restrict Istio Gateway hosts instead of using wildcard hosts in production overlays.
- [ ] Add `security/threat-model.md`, `security/supply-chain.md`, and `security/hardening-checklist.md`.

### Acceptance criteria

- [ ] No production-like manifest contains committed plaintext or base64 secrets.
- [ ] Workloads retrieve secrets through Azure Key Vault or encrypted GitOps flow.
- [ ] Network policies prevent unintended pod-to-pod and frontend-to-database traffic.
- [ ] CI fails when rendered manifests violate policy.
- [ ] Images have SBOMs and scan results.
- [ ] Istio mTLS and service-to-service authorization are demoable.
- [ ] Threat model documents protected assets, assumptions, and accepted risks.

### Demo moment

Show a policy-blocked pull request, demonstrate Key Vault secret delivery through workload identity, prove direct frontend-to-database traffic is denied, and show mTLS-protected service calls.

### Complexity traps to avoid

- Do not make policies so strict that local development stops working.
- Do not expose phpMyAdmin in production-like AKS overlays.
- Do not enable strict mTLS without checking health probes and readiness behavior.
- Do not introduce every supply-chain tool at once; SBOM plus scanning first, signing next.

## Suggested initial build order

1. README and docs polish.
2. `.env.example`, `.gitignore`, and Docker Compose profiles.
3. Container pinning and `.dockerignore` files.
4. Kustomize base/local overlay migration.
5. CI for language builds and Kubernetes validation.
6. Terraform AKS/ACR baseline.
7. Image release to ACR.
8. Flux GitOps bootstrap.
9. Observability health, metrics, dashboards, alerts, and runbooks.
10. Secrets, network policy, policy-as-code, and supply-chain hardening.

## Backlog parking lot

These are useful but should wait until the core milestones work:

- Helm chart packaging for the app.
- Full private AKS cluster with private endpoints.
- Multi-region active-active deployment.
- Advanced canary analysis.
- Chaos engineering automation.
- Managed database migration.
- End-to-end browser tests.
- Full OpenTelemetry instrumentation for every language.
