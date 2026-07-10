# AKS and GitOps Runbook

This is the step-by-step Milestone 2 path after the local Minikube flow works. The goal is to move the same simple TODO app into Azure without hiding the platform pieces.

## What gets created

| Layer | Repo location | What it does |
| --- | --- | --- |
| Azure infrastructure | `infra/terraform/` | Creates the resource group, virtual network, AKS cluster, ACR, Log Analytics, Key Vault, managed identities, and the GitHub Actions release identity. |
| Image release | `.github/workflows/acr-image-release.yaml` | Builds the five app images, creates SBOMs, pushes to ACR, and updates AKS Kustomize image tags. Vulnerability scanning is intentionally deferred until a maintained scanner action is selected. |
| AKS app manifests | `k8s/overlays/aks-dev` | Renders the production-shaped dev deployment that points at ACR images and AKS ingress settings. |
| GitOps cluster state | `clusters/aks-dev` and `gitops/` | Tells Flux what to reconcile into the dev AKS cluster, including ingress-nginx infrastructure and the app overlay. |

## Before you start

You need:

- Azure CLI logged in with `az login`.
- Terraform installed locally.
- GitHub CLI logged in with `gh auth login`.
- Flux CLI installed locally.
- Permission to create Azure resources and GitHub repository variables.

The commands below are written for PowerShell and should be run from the repository root. Set your subscription once before planning:

```powershell
az account set --subscription <subscription-id>
```

> Cost note: this creates real Azure resources. Use the dev tfvars first and destroy the environment when the demo is done.

## 1. Validate Terraform locally

```powershell
terraform -chdir=infra/terraform init -backend=false
terraform -chdir=infra/terraform fmt -check -recursive
terraform -chdir=infra/terraform validate
```

Checkpoint: validation should pass before you create Azure resources.

## 2. Plan the dev environment

```powershell
$env:TF_VAR_github_repository = (gh repo view --json nameWithOwner -q ".nameWithOwner").Trim()
terraform -chdir=infra/terraform init
terraform -chdir=infra/terraform plan `
  -var-file="environments/dev.tfvars" `
  -out="tfplan"
```

The `TF_VAR_github_repository` environment variable lets Terraform create the GitHub Actions OIDC trust for this repository without requiring a long `-var` argument in PowerShell. Without it, Azure resources can still be created, but the image release workflow will not be able to authenticate to Azure.

Checkpoint: review the plan for expected resource names, region, AKS node count, and ACR creation.

## 3. Apply after reviewing cost and scope

```powershell
terraform -chdir=infra/terraform apply "tfplan"
```

Checkpoint: Terraform should finish with outputs for the AKS cluster, ACR login server, and GitHub Actions release identity.

## 4. Configure GitHub repository variables from Terraform outputs

Run this from the repository root after apply:

```powershell
$GitHubVariableCommands = terraform -chdir=infra/terraform output -raw github_actions_variable_commands
$GitHubVariableCommands | Where-Object { $_.Trim() } | ForEach-Object {
    Invoke-Expression $_
}
```

If you are running from Git Bash instead, the equivalent is:

```bash
terraform -chdir=infra/terraform output -raw github_actions_variable_commands | bash
```

This writes the repository variables consumed by the ACR image release workflow:

- `AZURE_CLIENT_ID`
- `AZURE_TENANT_ID`
- `AZURE_SUBSCRIPTION_ID`
- `ACR_LOGIN_SERVER`

Checkpoint:

```powershell
gh variable list
```

## 5. Run the image release workflow

From GitHub Actions, run **ACR Image Release** manually for `aks-dev`, or push a change to one of the service build contexts on `master`.

The workflow should:

1. Build each app image.
2. Generate an SPDX SBOM artifact for each image.
3. Push images to ACR after SBOM generation.
4. Update the AKS overlay image tags.

Vulnerability scanning is intentionally deferred until a maintained scanner action is selected.

Checkpoint: confirm `k8s/overlays/aks-dev/kustomization.yaml` no longer uses placeholder tags after a successful release commit.

## 6. Connect kubectl to AKS

This Terraform stack enables AKS-managed Entra ID and Azure RBAC for the Kubernetes API. That means `az aks get-credentials` only writes a kubeconfig entry; your signed-in Azure principal must also have an Azure Kubernetes Service RBAC role assignment on the AKS cluster before `kubectl` can list or create cluster resources.

For a one-person dev environment, grant your current Azure CLI identity cluster-admin access at the AKS resource scope before the first `kubectl` check:

```powershell
$ResourceGroupName = terraform -chdir=infra/terraform output -raw resource_group_name
$AksClusterName = terraform -chdir=infra/terraform output -raw aks_cluster_name
$AksResourceId = az aks show `
  --resource-group $ResourceGroupName `
  --name $AksClusterName `
  --query id `
  --output tsv
$SignedInObjectId = az ad signed-in-user show --query id --output tsv
az role assignment create `
  --assignee-object-id $SignedInObjectId `
  --assignee-principal-type User `
  --role "Azure Kubernetes Service RBAC Cluster Admin" `
  --scope $AksResourceId
az aks get-credentials --resource-group $ResourceGroupName --name $AksClusterName --overwrite-existing
```

If `az ad signed-in-user show` is blocked by tenant permissions, use your user principal name instead:

```powershell
$SignedInUserPrincipalName = az account show --query user.name --output tsv
az role assignment create `
  --assignee $SignedInUserPrincipalName `
  --role "Azure Kubernetes Service RBAC Cluster Admin" `
  --scope $AksResourceId
```

For a shared team environment, prefer adding an Entra group object ID to `admin_group_object_ids` in the environment tfvars and assigning users through that group rather than granting direct per-user cluster-admin access.

Checkpoint:

```powershell
kubectl get nodes
```

## 7. Bootstrap Flux to the dev cluster

Export a GitHub token before bootstrapping so Flux can authenticate non-interactively from PowerShell. The `--private=false` flag matches this repository's public visibility; use `--private=true` for a private fork.

```powershell
$env:GITHUB_TOKEN = gh auth token
$GitHubOwner = gh repo view --json owner -q ".owner.login"

flux bootstrap github `
  --owner=$GitHubOwner `
  --repository=overkilled-todo-app `
  --branch=master `
  --path=clusters/aks-dev `
  --personal `
  --private=false
```

If you are running against a fork and already know the owner, setting `--owner=<github-owner>` directly is also fine.

Checkpoint:

```powershell
flux get sources git -A
flux get kustomizations -A
kubectl get helmrepository,helmrelease -n ingress-nginx
kubectl get pods,svc -n ingress-nginx
kubectl get deploy,svc,hpa,ingress -n todo-app
```

## 8. Reconcile after image releases

If you need Flux to pick up a new image-tag commit immediately:

```powershell
flux reconcile kustomization todo-app --namespace flux-system --with-source
kubectl get deploy,svc,hpa,ingress -n todo-app
```

## 9. Verify public ingress

The AKS overlay routes frontend traffic from `/` to `front-end` and API traffic from `/todo` to `api-gateway`. The ingress-nginx controller is reconciled from `gitops/infrastructure/ingress-nginx` and publishes an Azure LoadBalancer service.

```powershell
kubectl get svc ingress-nginx-controller -n ingress-nginx
$IngressIp = kubectl get svc ingress-nginx-controller -n ingress-nginx -o jsonpath="{.status.loadBalancer.ingress[0].ip}"
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/"
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/todo"
```

For a portfolio demo without paid DNS, testing the external IP with the `Host` header is enough to prove that the AKS overlay exposes both the frontend and API through ingress.

## Common problems

| Symptom | First thing to check |
| --- | --- |
| The image release workflow cannot log in to Azure. | Confirm `github_repository` was set during Terraform plan/apply and that `gh variable list` shows the four workflow variables. |
| ACR push is denied. | Confirm Terraform applied the `AcrPush` role assignment for the GitHub Actions release identity. |
| `kubectl get nodes` returns `Forbidden` for your user object ID after `az aks get-credentials`. | The kubeconfig is present, but your Azure principal does not have Kubernetes API authorization. Assign `Azure Kubernetes Service RBAC Cluster Admin` at the AKS resource scope as shown in step 6, then rerun `az aks get-credentials --overwrite-existing`. |
| Flux reconciles but pods cannot pull images. | Confirm AKS has `AcrPull` on the ACR and the overlay image names match the Terraform ACR login server. |
| Ingress does not return traffic. | Confirm `helmrelease/ingress-nginx` is ready, `svc/ingress-nginx-controller` has an external IP, and the Azure LoadBalancer health probe annotation is present on the controller service. |
| Some GitOps infrastructure folders look empty. | That is intentional for this milestone slice. Monitoring and ingress-nginx are active GitOps-managed add-ons; cert-manager, External Secrets, and policy remain placeholders for follow-up PRs. |
| Terraform warns that `azure_active_directory_role_based_access_control.managed` is deprecated. | This is expected with the pinned AzureRM 3.x provider. The field is still kept as `true` for managed Entra integration compatibility and should be removed when the Terraform stack is upgraded to AzureRM 4.x. |
| `terraform destroy` cannot delete `rg-oktodo-dev` because `Microsoft.OperationsManagement/solutions/ContainerInsights(log-oktodo-dev)` still exists. | Import the existing Container Insights solution into Terraform state using the recovery commands in the teardown section, then rerun destroy. This usually means the environment was created before the solution was explicitly tracked in Terraform state. |

## Tear down the dev environment

When you are done with the demo environment:

```powershell
terraform -chdir=infra/terraform destroy -var-file="environments/dev.tfvars"
```

If you created the GitHub OIDC trust with `TF_VAR_github_repository`, set the same environment variable during destroy:

```powershell
$env:TF_VAR_github_repository = (gh repo view --json nameWithOwner -q ".nameWithOwner").Trim()
terraform -chdir=infra/terraform destroy `
  -var-file="environments/dev.tfvars"
```

### Recover a failed destroy when Container Insights is still in the resource group

If destroy fails with a remaining resource like `Microsoft.OperationsManagement/solutions/ContainerInsights(log-oktodo-dev)`, the AKS monitoring add-on created or retained the Container Insights solution but your local Terraform state does not currently track it. Import that exact solution into state, then rerun destroy.

Before importing, confirm your checkout contains the Terraform resource block that the import target uses:

```powershell
Select-String `
  -Path infra/terraform/*.tf `
  -Pattern 'resource "azurerm_log_analytics_solution" "container_insights"'
```

If that command prints no match, update your local branch first because Terraform cannot import to an address that does not exist in configuration:

```powershell
git pull
terraform -chdir=infra/terraform init
```

Use the subscription ID, resource group, and workspace name from your error output. For the default dev names, the import ID shape is:

```powershell
terraform -chdir=infra/terraform import `
  -var-file="environments/dev.tfvars" `
  azurerm_log_analytics_solution.container_insights `
  "/subscriptions/<subscription-id>/resourceGroups/rg-oktodo-dev/providers/Microsoft.OperationsManagement/solutions/ContainerInsights(log-oktodo-dev)"

terraform -chdir=infra/terraform destroy `
  -var-file="environments/dev.tfvars"
```

For a real error, replace `<subscription-id>` with the subscription ID from your own Terraform or Azure CLI output, but do not commit account-specific IDs to this runbook. If Terraform says `resource address "azurerm_log_analytics_solution.container_insights" does not exist in the configuration`, your local checkout does not include the current Terraform configuration yet; pull the latest branch contents, rerun `terraform init`, and then rerun the import. Avoid setting `prevent_deletion_if_contains_resources = false` as the default fix because this stack is intended to explicitly manage and destroy the platform resources it creates.
