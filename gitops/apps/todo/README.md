# TODO GitOps App

Flux reconciles the TODO application from `k8s/overlays/aks-dev`. The image tags start as placeholders and are updated by the `ACR Image Release` workflow after it builds, publishes, and tags the service images in Azure Container Registry.

For the full operator flow, use `docs/aks-gitops-runbook.md` from the repository root.
