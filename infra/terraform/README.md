# Azure AKS Terraform

This directory provisions the Milestone 2 Azure foundation for the Overkill(ed) Todo App:

- Resource group
- Virtual network and AKS subnet
- AKS cluster with Azure AD RBAC, OIDC issuer, workload identity, and Log Analytics agent
- Azure Container Registry
- Log Analytics workspace
- Key Vault
- User-assigned managed identities reserved for External Secrets and Flux integrations

The Terraform is intentionally environment-focused instead of module-heavy. The goal is a readable portfolio demo that can be validated, planned, and applied by one operator.

## Prerequisites

- Terraform `>= 1.8`
- Azure CLI authenticated with `az login`
- An Azure subscription selected with `az account set --subscription <subscription-id>`
- Sufficient permissions to create AKS, ACR, Key Vault, managed identities, federated identity credentials, and role assignments
- GitHub CLI (`gh`) authenticated to the target repository if you want to copy Terraform outputs directly into repository variables

## Validate

```bash
terraform -chdir=infra/terraform init -backend=false
terraform -chdir=infra/terraform fmt -check -recursive
terraform -chdir=infra/terraform validate
```

## Plan Dev

```bash
terraform -chdir=infra/terraform init
terraform -chdir=infra/terraform plan \
  -var-file=environments/dev.tfvars \
  -var="github_repository=$(gh repo view --json nameWithOwner -q .nameWithOwner)" \
  -out=tfplan
```

The `github_repository` variable creates the GitHub Actions OIDC trust for the release workflow. Omit it only when you are provisioning Azure resources before the repository variable setup is ready.

## Apply Expectations

Applies are manual by design at this milestone. A reviewer should see the plan, confirm expected Azure cost and scope, then approve:

```bash
terraform -chdir=infra/terraform apply tfplan
```

Before applying, confirm:

- The selected subscription is correct.
- Resource names and region match the target environment.
- AKS node count and VM size are acceptable for demo cost.
- Key Vault purge protection is understood before destroying resources.

## After Provisioning

Get cluster credentials:

```bash
az aks get-credentials \
  --resource-group "$(terraform -chdir=infra/terraform output -raw resource_group_name)" \
  --name "$(terraform -chdir=infra/terraform output -raw aks_cluster_name)"
```

The ACR login server from `terraform output -raw acr_login_server` is used by the image release workflow and the AKS Kustomize overlays.

## Image Release Workflow

The `ACR Image Release` workflow publishes the five application service images to the ACR login server exported by Terraform. Terraform creates a dedicated user-assigned managed identity for this workflow, grants it `AcrPush` on the registry, and optionally creates the GitHub Actions federated credential when `github_repository` is set during plan/apply.

After `terraform apply`, run the generated GitHub CLI commands from the repository root to configure the variables consumed by the workflow:

```bash
terraform -chdir=infra/terraform output -raw github_actions_variable_commands | bash
```

The output sets these repository variables:

- `AZURE_CLIENT_ID`: Federated identity client ID with permission to push to ACR.
- `AZURE_TENANT_ID`: Azure tenant ID for the federated identity.
- `AZURE_SUBSCRIPTION_ID`: Azure subscription ID containing the ACR.
- `ACR_LOGIN_SERVER`: Value from `terraform output -raw acr_login_server`.

The workflow uses GitHub OIDC through `azure/login`, builds each image, creates an SPDX SBOM artifact, fails on high or critical Trivy findings, pushes only successfully scanned images, and updates the selected AKS Kustomize overlay image tags to the released SHA tag.
