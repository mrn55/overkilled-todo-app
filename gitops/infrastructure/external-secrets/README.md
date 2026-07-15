# External Secrets Operator

Flux installs External Secrets Operator from the upstream Helm chart so AKS overlays can materialize Kubernetes Secrets from Azure Key Vault instead of committing secret values to Git.

Before reconciling this add-on, replace the HelmRelease workload identity annotation value with the Terraform output:

```powershell
terraform -chdir=infra/terraform output -raw external_secrets_identity_client_id
```

The controller pods are labeled with `azure.workload.identity/use: "true"`. The AKS app overlays also annotate a namespaced `external-secrets` ServiceAccount with the same client ID, and Terraform creates the matching federated identity credential for `system:serviceaccount:todo-app:external-secrets`.

Verify after reconciliation:

```powershell
kubectl get helmrepository,helmrelease -n external-secrets
kubectl get pods -n external-secrets
```
