# AKS Dev Overlay

This overlay is the first AKS deployment target for Milestone 2. It keeps the same simple TODO app behavior while changing platform concerns:

- Images point at the dev Azure Container Registry login server from Terraform.
- Ingress uses an AKS-specific hostname and the `nginx` ingress class.
- Local-only `imagePullPolicy: Never` is not used.
- Gateway, frontend, and read-service have HPAs because they are stateless request handlers with resource requests.
- MariaDB remains in-cluster for demo cost control.

The image tags are intentionally set to `dev-placeholder` until the image release workflow updates them to immutable commit tags.

Render locally:

```bash
kubectl kustomize k8s/overlays/aks-dev
```

## Database Upgrade Path

The in-cluster MariaDB deployment keeps the AKS demo inexpensive and easy to tear down. A production-grade path should replace it with Azure Database for MySQL Flexible Server, private networking, managed backups, and External Secrets-sourced credentials before this overlay is treated as production.
