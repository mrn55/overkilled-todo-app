# Observability

Milestone 3 makes the TODO platform operable with health checks, metrics, dashboards, alerts, SLOs, runbooks, and load demos.

## AKS demo flow

After Flux reconciles `gitops/infrastructure/monitoring`, verify the stack:

```powershell
flux get kustomizations -A
kubectl get helmrepository,helmrelease -n monitoring
kubectl get pods,svc -n monitoring
kubectl get servicemonitor,prometheusrule -A
```

Open Grafana with a local port-forward:

```powershell
kubectl port-forward svc/monitoring-grafana -n monitoring 3000:80
```

Use `admin` / `prom-operator` for the demo admin login. The dashboards are loaded from ConfigMaps labeled `grafana_dashboard=1`.

Generate traffic:

```powershell
$env:BASE_URL = "http://todo-dev.example.com"
k6 run observability/load/k6-smoke.js
k6 run observability/load/k6-load.js
```

If testing against a LoadBalancer IP before DNS is configured, set `HOST_HEADER`:

```powershell
$env:BASE_URL = "http://<ingress-ip>"
$env:HOST_HEADER = "todo-dev.example.com"
k6 run observability/load/k6-smoke.js
```

## Failure demo

Use the database-unavailable scenario:

```powershell
kubectl scale deployment/mariadb -n todo-app --replicas=0
```

Watch Grafana and Alertmanager for `TodoDatabaseUnavailable`, then follow `observability/runbooks/database-unavailable.md` to restore service.

Tracing is intentionally deferred until the metrics, dashboards, alerts, and runbooks are useful.

## Azure managed monitoring path

The in-cluster stack is the first Milestone 3 target because it is self-contained for a portfolio demo. A production AKS environment should evaluate Azure Managed Prometheus and Azure Managed Grafana instead of owning the full monitoring stack in-cluster.

Production-shaped follow-up work:

- Enable Azure Monitor managed Prometheus during AKS provisioning or as an AKS add-on.
- Connect Azure Managed Grafana to the managed Prometheus workspace.
- Reuse the dashboard JSON and alert/runbook naming from this directory where possible.
- Keep application scrape intent in GitOps, but let Azure own Prometheus and Grafana lifecycle, patching, and storage.
