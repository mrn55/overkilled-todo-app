# ingress-nginx

Flux installs ingress-nginx from the upstream Helm chart and exposes it through an Azure LoadBalancer service. The AKS app overlay uses `ingressClassName: nginx`, so this controller is required before `todo-ingress` can serve public frontend or API traffic.

The controller service includes the AKS health probe path annotation:

```yaml
service.beta.kubernetes.io/azure-load-balancer-health-probe-request-path: /healthz
```

Verify after reconciliation:

```powershell
flux reconcile kustomization flux-system --namespace flux-system --with-source
kubectl get helmrepository,helmrelease -n ingress-nginx
kubectl get pods,svc -n ingress-nginx
kubectl get ingress -n todo-app
```

When the controller service has an external IP, test the app ingress with the overlay host:

```powershell
$IngressIp = kubectl get svc ingress-nginx-controller -n ingress-nginx -o jsonpath="{.status.loadBalancer.ingress[0].ip}"
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/"
curl.exe -H "Host: todo-dev.example.com" "http://$IngressIp/todo"
```
