# Minikube local Kubernetes runbook

This runbook validates the local Kubernetes path for the Overkill(ed) Todo App. The local environment renders from `k8s/overlays/local`, keeps Minikube-only image names and `imagePullPolicy: Never` out of the reusable base, and deploys application resources into the `todo-app` namespace.

Milestone 3 also adds health, readiness, metrics, and optional local observability checks. Minikube can prove most of that behavior before AKS, but it does not prove Azure LoadBalancer behavior, ACR image pulls, Flux bootstrap, or Azure managed monitoring.

## Prerequisites

- Docker Engine or Docker Desktop.
- Minikube.
- `kubectl` configured for the Minikube context.
- Optional: Flux CLI for testing the GitOps-managed monitoring stack locally.
- Optional: k6 for the load scripts under `observability/load`.

The commands below are written for PowerShell from the repository root.

## Start the cluster

Use a slightly larger local cluster if you plan to test the monitoring stack. The app alone can run with less, but Prometheus and Grafana need more headroom.

```powershell
minikube start --driver=docker --cpus=4 --memory=8192
minikube addons enable ingress
minikube addons enable metrics-server
kubectl cluster-info
```

Confirm the context before applying anything:

```powershell
kubectl config current-context
```

Expected context: `minikube`.

## Build local images

Build the local service images with Docker Compose, then load them into Minikube with the exact names used by `k8s/overlays/local`:

```powershell
docker compose build create-service read-service update-service delete-service todo-frontend
minikube image load overkilled-todo-app-create-service:latest
minikube image load overkilled-todo-app-read-service:latest
minikube image load overkilled-todo-app-update-service:latest
minikube image load overkilled-todo-app-delete-service:latest
minikube image load overkilled-todo-app-todo-frontend:latest
```

`minikube image build` can work too, but it is more sensitive to driver and path behavior on Windows. If you use it, pass the build context directory, not the Dockerfile path:

```powershell
minikube image build -t overkilled-todo-app-todo-frontend:latest ./todo-frontend
minikube image build -t overkilled-todo-app-create-service:latest ./create-service-haskell
minikube image build -t overkilled-todo-app-read-service:latest ./read-service-go
minikube image build -t overkilled-todo-app-update-service:latest ./update-service-rust
minikube image build -t overkilled-todo-app-delete-service:latest ./delete-service-erlang
```

## Render and apply

Render first when you want to inspect the exact resources:

```powershell
kubectl kustomize k8s/overlays/local
```

Apply the local overlay:

```powershell
kubectl apply -k k8s/overlays/local
```

## Verify rollout

```powershell
kubectl get pods,svc,ingress -n todo-app
kubectl rollout status deployment/mariadb -n todo-app
kubectl rollout status deployment/create-service -n todo-app
kubectl rollout status deployment/read-service -n todo-app
kubectl rollout status deployment/update-service -n todo-app
kubectl rollout status deployment/delete-service -n todo-app
kubectl rollout status deployment/front-end -n todo-app
kubectl rollout status deployment/api-gateway -n todo-app
```

If a pod is not ready, inspect the probe and event output:

```powershell
kubectl describe pod <pod-name> -n todo-app
kubectl logs deployment/<deployment-name> -n todo-app
```

## Access the app

The most reliable local path is port-forwarding:

```powershell
kubectl port-forward svc/front-end -n todo-app 3000:80
kubectl port-forward svc/api-gateway -n todo-app 8081:80
```

Open:

- Frontend: <http://localhost:3000>
- API gateway: <http://localhost:8081/todo>

The frontend defaults to the port-forwarded gateway URL when served from `localhost:3000`, which avoids browser calls to an un-forwarded `http://localhost/todo` endpoint.

You can also test the Minikube ingress with a Host header:

```powershell
$MinikubeIp = minikube ip
curl.exe -H "Host: localhost" "http://$MinikubeIp/"
curl.exe -H "Host: localhost" "http://$MinikubeIp/todo"
```

If your driver does not expose ingress directly, keep using the port-forward path.

## Health and readiness checks

Gateway checks:

```powershell
curl.exe http://127.0.0.1:8081/healthz
curl.exe http://127.0.0.1:8081/readyz
curl.exe http://127.0.0.1:8081/todo
```

Backend readiness can be checked with service port-forwards:

```powershell
kubectl port-forward svc/create-service -n todo-app 5001:5001
kubectl port-forward svc/read-service -n todo-app 5002:5002
kubectl port-forward svc/update-service -n todo-app 5003:5003
kubectl port-forward svc/delete-service -n todo-app 5004:5004
```

Then, from another terminal:

```powershell
curl.exe http://127.0.0.1:5001/readyz
curl.exe http://127.0.0.1:5002/readyz
curl.exe http://127.0.0.1:5003/readyz
curl.exe http://127.0.0.1:5004/readyz
```

## Metrics checks

The gateway service exposes the NGINX exporter sidecar on port `9113`:

```powershell
kubectl port-forward svc/api-gateway -n todo-app 9113:9113
curl.exe http://127.0.0.1:9113/metrics
```

The read service exposes application metrics on its HTTP port:

```powershell
kubectl port-forward svc/read-service -n todo-app 5002:5002
curl.exe http://127.0.0.1:5002/metrics
```

Generate a few gateway/read requests, then check for metrics such as:

- `nginx_http_requests_total`
- `todo_read_service_http_requests_total`
- `todo_read_service_http_request_duration_seconds`
- `todo_read_service_db_failures_total`

## Optional: local monitoring stack

The monitoring stack is stored as Flux Helm resources under `gitops/infrastructure/monitoring`. To test it in Minikube, install Flux controllers first so the `HelmRepository` and `HelmRelease` kinds exist:

```powershell
flux install
kubectl apply -k gitops/infrastructure/monitoring
kubectl get helmrepository,helmrelease -n monitoring
kubectl get pods,svc -n monitoring
kubectl get servicemonitor,prometheusrule -A
```

This requires internet access from the cluster so Flux can pull the `kube-prometheus-stack` chart from the Prometheus Community Helm repository.

Open Grafana:

```powershell
kubectl port-forward svc/monitoring-grafana -n monitoring 3000:80
```

Login:

```text
admin / prom-operator
```

The dashboards are loaded from ConfigMaps labeled `grafana_dashboard=1`.

## Optional: k6 load demo

With the gateway port-forward running on `8081`:

```powershell
$env:BASE_URL = "http://127.0.0.1:8081"
k6 run observability/load/k6-smoke.js
k6 run observability/load/k6-load.js
```

If testing through ingress instead:

```powershell
$env:BASE_URL = "http://$(minikube ip)"
$env:HOST_HEADER = "localhost"
k6 run observability/load/k6-smoke.js
```

## Failure demo

To test readiness, alerts, and the database runbook locally, scale MariaDB down:

```powershell
kubectl scale deployment/mariadb -n todo-app --replicas=0
kubectl get pods -n todo-app
curl.exe http://127.0.0.1:8081/todo
```

Restore it:

```powershell
kubectl scale deployment/mariadb -n todo-app --replicas=1
kubectl rollout status deployment/mariadb -n todo-app
```

If the monitoring stack is running, watch for `TodoDatabaseUnavailable` and follow `observability/runbooks/database-unavailable.md`.

## Troubleshooting

Useful inspection commands:

```powershell
kubectl get events -n todo-app --sort-by=.lastTimestamp
kubectl describe pod <pod-name> -n todo-app
kubectl logs deployment/api-gateway -n todo-app
kubectl logs deployment/read-service -n todo-app
kubectl logs deployment/create-service -n todo-app
kubectl logs deployment/update-service -n todo-app
kubectl logs deployment/delete-service -n todo-app
```

Database shell:

```powershell
kubectl exec -it deployment/mariadb -n todo-app -- mysql -uroot -p
```

Local-development database credentials are still committed Kubernetes secret values in this milestone. Milestone 4 replaces them with an enterprise secret-management path.

Common issues:

| Symptom | First check |
| --- | --- |
| `ImagePullBackOff` for app services | Rebuild with `docker compose build`, reload with `minikube image load`, and confirm the local overlay uses `imagePullPolicy: Never`. |
| Backend pods are running but not ready | Check `/readyz` and confirm MariaDB is ready. |
| Gateway is ready but `/todo` fails | Check backend rollout status and gateway logs. |
| Metrics port-forward fails | Confirm `svc/api-gateway` has the `metrics` port and the `nginx-exporter` container is running. |
| Monitoring HelmRelease is not ready | Check `kubectl describe helmrelease kube-prometheus-stack -n monitoring` and Flux controller logs. |

## Clean up

Remove the app:

```powershell
kubectl delete -k k8s/overlays/local --ignore-not-found
```

Remove optional monitoring:

```powershell
kubectl delete -k gitops/infrastructure/monitoring --ignore-not-found
```

Delete the cluster when finished:

```powershell
minikube delete
```
