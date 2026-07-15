# Demo Script

Use this walkthrough for recruiters, interviewers, or reviewers. The goal is to show platform engineering judgment around a deliberately small application and then connect the local repo story to the AKS/GitOps runbook in `docs/aks-gitops-runbook.md`.

## Demo goals

- Keep the TODO CRUD behavior intentionally simple.
- Show that the platform around the app is the portfolio piece: local reproducibility, Kubernetes shape, CI validation, Azure infrastructure, GitOps, observability, and security.
- Make every command or screen share answer the question: "Could this person build and operate a production-shaped platform?"

## 1. Position the project

**Narration:**

> This is an intentionally over-engineered TODO app. The business domain is boring on purpose; the project is meant to show platform engineering judgment around a small polyglot workload.

Show:

- `README.md` for the portfolio overview.
- `docs/platform-milestones.md` for the phased roadmap.
- `docs/architecture.md` for the service topology.

Call out:

- Milestone 1 proves local reproducibility, container hardening, Kubernetes shape, and CI gates.
- Milestone 2 moves the same app into Azure with Terraform, ACR, AKS, and Flux.
- Milestone 3 adds dashboards, alerts, SLOs, load tests, and runbooks.
- Milestone 4 is the enterprise security path: Key Vault, workload identity, network policy, admission policy, and supply-chain controls.

## 2. Run the app locally first

```sh
cp .env.example .env
docker compose up --build -d
docker compose ps
```

Open:

- Frontend: <http://localhost:3000>
- API gateway: <http://localhost/todo>

Demo moment:

- Create, update, read, and delete one TODO item.
- Keep this section short; the point is not product depth, it is proving the app is real before showing the platform around it.

## 3. Show service boundaries

Show:

- `docs/architecture.md`
- `api-gateway.conf`
- One backend service directory, such as `read-service-go/` or `update-service-rust/`

Narration points:

- NGINX routes requests by method to separate CRUD services.
- The services are intentionally polyglot so CI, containers, health checks, and runtime operations look more realistic than a single-stack toy app.
- The gateway and services expose health/readiness behavior so Kubernetes and monitoring can reason about them.

## 4. Show local platform shape

Show:

- `docker-compose.yml` for local service wiring, health checks, profiles, and pinned images.
- `k8s/base` and `k8s/overlays/local` for the Kustomize structure.
- `.github/workflows/` for CI validation.

Suggested commands:

```sh
git diff --check
kustomize build k8s/overlays/local
```

Narration points:

- Local Compose is for fast development.
- Kustomize is the promotion path from local manifests to AKS overlays.
- CI validates rendered Kubernetes and container build hygiene so platform changes are reviewable.

## 5. Transition to the Azure/AKS story

**Narration:**

> After proving the app locally, the AKS/GitOps runbook is the cloud deployment path. It is intentionally explicit so reviewers can see the Azure resources, release identity, image promotion flow, and Flux reconciliation instead of a black-box script.

Open `docs/aks-gitops-runbook.md` and show the "What gets created" table.

Call out the four layers:

1. Terraform provisions Azure infrastructure under `infra/terraform/`.
2. GitHub Actions builds images, creates SBOMs, pushes to ACR, and updates image tags.
3. `k8s/overlays/aks-dev` renders the AKS deployment shape.
4. `clusters/aks-dev` and `gitops/` let Flux reconcile infrastructure add-ons and the app.

## 6. Walk through the AKS/GitOps runbook checkpoints

Do not spend the video reading every command. Use the runbook as a checklist and show the successful checkpoints.

### Terraform validation and plan

Show these runbook sections:

- "Validate Terraform locally"
- "Plan the dev environment"
- "Apply after reviewing cost and scope"

Suggested checkpoint commands:

```powershell
terraform -chdir=infra/terraform fmt -check -recursive
terraform -chdir=infra/terraform validate
terraform -chdir=infra/terraform plan -var-file="environments/dev.tfvars"
```

Narration points:

- The Terraform stack creates AKS, ACR, Log Analytics, Key Vault, identities, and the GitHub Actions OIDC release identity.
- The plan is reviewed before apply because it creates real Azure resources.

### GitHub Actions image release

Show these runbook sections:

- "Configure GitHub repository variables from Terraform outputs"
- "Run the image release workflow"

Demo moment:

- Open the **ACR Image Release** workflow run.
- Show image build steps, SBOM artifacts, and the commit that updates `k8s/overlays/aks-dev/kustomization.yaml` image tags.

Narration points:

- GitHub Actions authenticates to Azure through OIDC, not long-lived cloud credentials.
- A successful release changes desired state in Git rather than requiring manual `kubectl set image` commands.

### Flux bootstrap and reconciliation

Show these runbook sections:

- "Connect kubectl to AKS"
- "Prepare database secrets in Key Vault"
- "Bootstrap Flux to the dev cluster"
- "Reconcile after image releases"

Suggested checkpoint commands:

```powershell
kubectl get nodes
flux get sources git -A
flux get kustomizations -A
kubectl get deploy,svc,hpa,ingress -n todo-app
```

Narration points:

- Flux watches `clusters/aks-dev` and applies the desired state from Git.
- External Secrets bridges Key Vault values into the Kubernetes `db-secret` expected by the app.
- The app overlay includes AKS-specific ingress, replicas, resource settings, and HPAs without changing the simple TODO behavior.

### Public ingress verification

Show the runbook section "Verify public ingress".

Suggested checkpoint commands:

```powershell
kubectl get svc ingress-nginx-controller -n ingress-nginx
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/"
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/todo"
```

Demo moment:

- Open the app through the ingress IP using the expected host header or browser setup.
- Create one TODO item to prove the end-to-end AKS path works.

Narration point:

- The demo uses an ingress IP plus host header so the portfolio does not require paid DNS just to prove the AKS path.

## 7. Show operations and troubleshooting maturity

Show:

- `observability/dashboards/`
- `observability/slos/todo-api-slo.md`
- `observability/runbooks/`
- The "Common problems" section in `docs/aks-gitops-runbook.md`

Optional demo commands if the AKS environment is live:

```powershell
kubectl get pods -n todo-app
kubectl describe externalsecret db-secret -n todo-app
kubectl logs deploy/api-gateway -n todo-app
```

Narration points:

- The runbook includes likely failure points: Azure auth, ACR permissions, Flux reconciliation, image pulls, ingress, and External Secrets.
- Milestone 3 adds golden-signal dashboards, alerts, SLOs, and incident runbooks so the platform is operable, not just deployable.

## 8. Close with the roadmap and next implementation step

Close on `docs/platform-milestones.md`.

Summary narration:

> The important part is that the same deliberately small app moves through local development, CI validation, Kubernetes rendering, Azure infrastructure, image release, GitOps reconciliation, and operational runbooks. The TODO app stays simple so the platform engineering work is easy to evaluate.

Next recommended implementation step:

- Continue Milestone 4 by adding policy-as-code validation for rendered manifests, starting with rules that block privileged containers, missing resource limits, and unsafe production image tags.

## Optional local developer tooling

Use this only if someone asks how the database is inspected locally:

```sh
docker compose --profile tools up -d
```

Open phpMyAdmin at <http://localhost:8080> as a local-only development aid. Do not present phpMyAdmin as part of the AKS production-shaped path.
