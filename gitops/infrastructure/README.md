# GitOps Infrastructure

These directories hold the GitOps structure for cluster add-ons required by Milestone 2 and later milestones:

- `ingress-nginx`: public ingress controller for the AKS dev overlay, installed through Flux and Helm.
- `cert-manager`: TLS certificate automation.
- `external-secrets`: Azure Key Vault-backed secret delivery.
- `policy`: admission policy controllers and policy bundles.
- `monitoring`: observability stack or Azure managed monitoring integration.

The non-ingress directories are lightweight placeholders so Flux can reconcile the structure before each controller is installed in focused follow-up PRs.
