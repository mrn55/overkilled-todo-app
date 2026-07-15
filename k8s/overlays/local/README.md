# Local Kubernetes overlay

The local overlay creates the `db-secret` Kubernetes Secret with Kustomize `secretGenerator` and `disableNameSuffixHash: true`, so the generated Secret keeps the stable name expected by MariaDB and the CRUD service manifests.

The checked-in values are local-only development defaults. Do not reuse them for shared clusters or AKS. To change them for your own Minikube session, edit the literals in this overlay before running:

```bash
kubectl apply -k k8s/overlays/local
kubectl get secret db-secret -n todo-app
```

AKS overlays do not use these local values. They use External Secrets Operator and Azure Key Vault instead.
