# Minikube local Kubernetes runbook

This runbook is for local validation while Milestone 1 moves the repo toward a production-shaped Kubernetes foundation. The current manifests are still flat files under `k8s/`; the next step is to promote them into `k8s/base` and `k8s/overlays/local` with Kustomize.

## Prerequisites

- Docker Engine.
- Minikube.
- `kubectl` configured for the Minikube context.
- Docker Compose if you want to build images with the same names used by local manifests.

## Start the cluster

```sh
minikube start --driver=docker
minikube addons enable ingress
kubectl cluster-info
```

## Build and load local images

Build the local service images, then load them into Minikube:

```sh
docker compose build create-service read-service update-service delete-service todo-frontend
minikube image load overkilled-todo-app-create-service:latest
minikube image load overkilled-todo-app-read-service:latest
minikube image load overkilled-todo-app-update-service:latest
minikube image load overkilled-todo-app-delete-service:latest
minikube image load overkilled-todo-app-todo-frontend:latest
```

## Apply the current manifests

```sh
kubectl apply -f k8s/database-manifest.yaml
kubectl apply -f k8s/create-service-manifest.yaml
kubectl apply -f k8s/read-service-manifest.yaml
kubectl apply -f k8s/update-service-manifest.yaml
kubectl apply -f k8s/delete-service-manifest.yaml
kubectl apply -f k8s/front-end-manifest.yaml
kubectl apply -f k8s/api-gateway-manifest.yaml
kubectl apply -f k8s/ingress-manifest.yaml
```

## Verify rollout

```sh
kubectl get pods,svc,ingress
kubectl rollout status deployment/create-service
kubectl rollout status deployment/read-service
kubectl rollout status deployment/update-service
kubectl rollout status deployment/delete-service
kubectl rollout status deployment/todo-frontend
kubectl rollout status deployment/api-gateway
```

If ingress host mapping is not configured locally, use port-forwarding:

```sh
kubectl port-forward svc/todo-frontend 3000:80
kubectl port-forward svc/api-gateway 8081:80
```

Then open <http://localhost:3000> for the frontend and <http://localhost:8081/todo> for the API gateway.

## Troubleshooting

```sh
kubectl describe pod <pod-name>
kubectl logs deployment/api-gateway
kubectl logs deployment/read-service
kubectl exec -it deployment/mariadb -- mysql -uroot -p
```

Database credentials are local-development values today. Milestone 4 will replace committed Kubernetes secrets with an enterprise secret-management path.

## Clean up

```sh
kubectl delete -f k8s/ingress-manifest.yaml --ignore-not-found
kubectl delete -f k8s/api-gateway-manifest.yaml --ignore-not-found
kubectl delete -f k8s/front-end-manifest.yaml --ignore-not-found
kubectl delete -f k8s/delete-service-manifest.yaml --ignore-not-found
kubectl delete -f k8s/update-service-manifest.yaml --ignore-not-found
kubectl delete -f k8s/read-service-manifest.yaml --ignore-not-found
kubectl delete -f k8s/create-service-manifest.yaml --ignore-not-found
kubectl delete -f k8s/database-manifest.yaml --ignore-not-found
minikube delete
```
