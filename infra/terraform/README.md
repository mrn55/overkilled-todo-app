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
- Sufficient permissions to create AKS, ACR, Key Vault, managed identities, and role assignments

## Validate

```bash
terraform -chdir=infra/terraform init -backend=false
terraform -chdir=infra/terraform fmt -check -recursive
terraform -chdir=infra/terraform validate
```

## Plan Dev

```bash
terraform -chdir=infra/terraform init
terraform -chdir=infra/terraform plan -var-file=environments/dev.tfvars -out=tfplan
```

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
