# Azure AKS Terraform

This directory provisions the Milestone 2 Azure foundation for the Overkill(ed) Todo App:

- Resource group
- Virtual network and AKS subnet
- AKS cluster with Azure AD RBAC, OIDC issuer, workload identity, and Log Analytics agent
- Azure Container Registry
- Log Analytics workspace and the Container Insights solution used by the AKS Log Analytics agent
- Key Vault
- User-assigned managed identities reserved for External Secrets and Flux integrations

The Terraform is intentionally environment-focused instead of module-heavy. The goal is a readable portfolio demo that can be validated, planned, and applied by one operator. If you want the complete end-to-end sequence, start with `docs/aks-gitops-runbook.md` from the repository root and use this file as the Terraform reference.

## Prerequisites

- Terraform `>= 1.8`
- Azure CLI authenticated with `az login`
- An Azure subscription selected with `az account set --subscription <subscription-id>`
- Sufficient permissions to create AKS, ACR, Key Vault, managed identities, federated identity credentials, and role assignments
- GitHub CLI (`gh`) authenticated to the target repository if you want to copy Terraform outputs directly into repository variables

## Validate

```powershell
terraform -chdir=infra/terraform init -backend=false
terraform -chdir=infra/terraform fmt -check -recursive
terraform -chdir=infra/terraform validate
```

## Plan Dev

```powershell
$env:TF_VAR_github_repository = (gh repo view --json nameWithOwner -q ".nameWithOwner").Trim()
terraform -chdir=infra/terraform init
terraform -chdir=infra/terraform plan `
  -var-file="environments/dev.tfvars" `
  -out="tfplan"
```

The `TF_VAR_github_repository` environment variable creates the GitHub Actions OIDC trust for the release workflow without requiring a long `-var` argument in PowerShell. Omit it only when you are provisioning Azure resources before the repository variable setup is ready.

## Apply Expectations

Applies are manual by design at this milestone. A reviewer should see the plan, confirm expected Azure cost and scope, then approve:

```powershell
terraform -chdir=infra/terraform apply "tfplan"
```

Before applying, confirm:

- The selected subscription is correct.
- Resource names and region match the target environment.
- AKS node count and VM size are acceptable for demo cost.
- Key Vault purge protection is understood before destroying resources.
- The AzureRM 3.x deprecation warning for `azure_active_directory_role_based_access_control.managed` is expected; the field remains set to `true` for managed Entra integration compatibility until this stack upgrades to AzureRM 4.x.

## Destroy Dev

The AKS `oms_agent` add-on requires the Log Analytics `ContainerInsights` solution. This stack manages that solution explicitly so `terraform destroy` can remove it before deleting the resource group instead of leaving Azure-created nested resources behind.

If you created the dev environment before this resource was added and `terraform destroy` already failed with `Microsoft.OperationsManagement/solutions/ContainerInsights(...)` still in the resource group, recover by importing the existing solution into state and then rerunning destroy:

```powershell
terraform -chdir=infra/terraform import `
  -var-file="environments/dev.tfvars" `
  azurerm_log_analytics_solution.container_insights `
  "/subscriptions/<subscription-id>/resourceGroups/rg-oktodo-dev/providers/Microsoft.OperationsManagement/solutions/ContainerInsights(log-oktodo-dev)"

terraform -chdir=infra/terraform destroy -var-file="environments/dev.tfvars"
```

Use the subscription ID, resource group name, and workspace name from the failing destroy output. Avoid setting `prevent_deletion_if_contains_resources = false` as the default fix because this Terraform is intended to account for all platform resources it creates.

## After Provisioning

Get cluster credentials:

```powershell
$ResourceGroupName = terraform -chdir=infra/terraform output -raw resource_group_name
$AksClusterName = terraform -chdir=infra/terraform output -raw aks_cluster_name
az aks get-credentials --resource-group $ResourceGroupName --name $AksClusterName
```

The ACR login server from `terraform output -raw acr_login_server` is used by the image release workflow and the AKS Kustomize overlays.

## Image Release Workflow

The `ACR Image Release` workflow publishes the five application service images to the ACR login server exported by Terraform. Terraform creates a dedicated user-assigned managed identity for this workflow, grants it `AcrPush` on the registry, and optionally creates the GitHub Actions federated credential when `github_repository` is set during plan/apply.

After `terraform apply`, run the generated GitHub CLI commands from the repository root to configure the variables consumed by the workflow:

```powershell
$GitHubVariableCommands = terraform -chdir=infra/terraform output -raw github_actions_variable_commands
$GitHubVariableCommands | Where-Object { $_.Trim() } | ForEach-Object {
    Invoke-Expression $_
}
```

The output sets these repository variables:

- `AZURE_CLIENT_ID`: Federated identity client ID with permission to push to ACR.
- `AZURE_TENANT_ID`: Azure tenant ID for the federated identity.
- `AZURE_SUBSCRIPTION_ID`: Azure subscription ID containing the ACR.
- `ACR_LOGIN_SERVER`: Value from `terraform output -raw acr_login_server`.

The workflow uses GitHub OIDC through `azure/login`, builds each image, creates an SPDX SBOM artifact, pushes the images to ACR, and updates the selected AKS Kustomize overlay image tags to the released SHA tag. Vulnerability scanning is intentionally deferred until a maintained scanner action is selected.
