# Runbook: High Error Rate or Latency

## Alert

- `TodoHighIngress5xxRate`
- `TodoHighIngressLatency`

## Impact

Users may see failed or slow TODO API calls through the public ingress.

## Triage

```powershell
kubectl get ingress -n todo-app
kubectl get pods,svc -n todo-app
kubectl get pods,svc -n ingress-nginx
kubectl logs deployment/api-gateway -n todo-app --tail=100
kubectl logs deployment/ingress-nginx-controller -n ingress-nginx --tail=100
```

Check Grafana:

- `TODO Overview` for ingress request rate, 5xx ratio, and latency.
- `Service Golden Signals` for read-service latency and pod resources.

## Recovery

1. If pods are not ready, inspect the failing deployment and follow `pod-crashloop.md`.
2. If MariaDB is unavailable, follow `database-unavailable.md`.
3. If ingress-nginx is unhealthy, reconcile the GitOps infrastructure and inspect the HelmRelease:

```powershell
flux reconcile kustomization flux-system -n flux-system --with-source
kubectl describe helmrelease ingress-nginx -n ingress-nginx
```

## Verify

```powershell
$IngressIp = kubectl get svc ingress-nginx-controller -n ingress-nginx -o jsonpath="{.status.loadBalancer.ingress[0].ip}"
curl.exe -i -H "Host: todo-dev.example.com" "http://$IngressIp/todo"
```
