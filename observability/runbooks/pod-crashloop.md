# Runbook: Pod CrashLoop or Low Replica Availability

## Alert

- `TodoPodCrashLooping`
- `TodoLowReplicaAvailability`

## Impact

One or more TODO services may be unavailable or running with reduced redundancy.

## Triage

```powershell
kubectl get deploy,pods -n todo-app
kubectl get events -n todo-app --sort-by=.lastTimestamp
kubectl describe pod <pod-name> -n todo-app
kubectl logs <pod-name> -n todo-app --previous
kubectl logs <pod-name> -n todo-app
```

Check recent GitOps changes:

```powershell
flux get kustomizations -A
git log --oneline --max-count=5
```

## Recovery

1. If the failure was caused by a bad image tag, revert the GitOps image-tag commit or rerun the image release workflow with a known-good commit.
2. If the failure was caused by a dependency outage, restore the dependency first.
3. Restart the affected deployment after the cause is fixed:

```powershell
kubectl rollout restart deployment/<deployment-name> -n todo-app
kubectl rollout status deployment/<deployment-name> -n todo-app
```

## Verify

```powershell
kubectl get deploy,pods -n todo-app
kubectl get ingress -n todo-app
```
