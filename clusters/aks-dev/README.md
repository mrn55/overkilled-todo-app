# AKS Dev Cluster State

This directory is the Flux bootstrap target for the dev AKS cluster. For the full Azure, ACR image release, and Flux sequence, start with `docs/aks-gitops-runbook.md` from the repository root.

After Terraform creates AKS and the operator has cluster credentials, bootstrap Flux against this path:

```bash
flux bootstrap github \
  --owner=<github-owner> \
  --repository=overkilled-todo-app \
  --branch=master \
  --path=clusters/aks-dev \
  --personal
```

Common checks:

```bash
flux get sources git -A
flux get kustomizations -A
flux reconcile kustomization todo-app --namespace flux-system --with-source
kubectl get ingress,deploy,hpa -n todo-app
```

The app reconciliation points to `k8s/overlays/aks-dev`. Infrastructure directories are placeholders for milestone follow-up PRs that install ingress-nginx, cert-manager, External Secrets, policy, and monitoring controllers.
