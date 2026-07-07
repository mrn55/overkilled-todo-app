# Runbook: Database Unavailable

## Alert

- `TodoDatabaseUnavailable`

## Impact

CRUD services cannot reliably read or write TODO items.

## Triage

```powershell
kubectl get deploy,pod,svc -n todo-app -l app.kubernetes.io/name=mariadb
kubectl describe deployment/mariadb -n todo-app
kubectl logs deployment/mariadb -n todo-app --tail=100
kubectl get events -n todo-app --sort-by=.lastTimestamp
```

Check whether the deployment was intentionally scaled down for a demo:

```powershell
kubectl get deployment/mariadb -n todo-app -o jsonpath="{.spec.replicas}"
```

## Recovery

Restore the expected single MariaDB replica:

```powershell
kubectl scale deployment/mariadb -n todo-app --replicas=1
kubectl rollout status deployment/mariadb -n todo-app
```

If the pod does not become ready, inspect volume, image, and secret references:

```powershell
kubectl describe pod -n todo-app -l app=mariadb
kubectl get secret db-secret -n todo-app
```

## Verify

```powershell
kubectl get pods -n todo-app
$IngressIp = kubectl get svc ingress-nginx-controller -n ingress-nginx -o jsonpath="{.status.loadBalancer.ingress[0].ip}"
curl.exe -i -H "Host: todo-dev.example.com" "http://$IngressIp/todo"
```
