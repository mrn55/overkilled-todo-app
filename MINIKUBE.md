# Minikube local Kubernetes runbook

This runbook is for local validation while Milestone 1 moves the repo toward a production-shaped Kubernetes foundation. The local environment now renders from the Kustomize overlay at `k8s/overlays/local`, which keeps Minikube-specific image names and `imagePullPolicy: Never` outside the reusable base.

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

## Preview and apply the local overlay

Render the overlay first when you want to inspect the exact resources that will be applied:

```sh
kustomize build k8s/overlays/local
```

Apply the local overlay to Minikube:

```sh
kubectl apply -k k8s/overlays/local
```

## Verify rollout

```sh
kubectl get pods,svc,ingress
kubectl rollout status deployment/create-service
kubectl rollout status deployment/read-service
kubectl rollout status deployment/update-service
kubectl rollout status deployment/delete-service
kubectl rollout status deployment/front-end
kubectl rollout status deployment/api-gateway
```

If ingress host mapping is not configured locally, use port-forwarding:

```sh
kubectl port-forward svc/front-end 3000:80
kubectl port-forward svc/api-gateway 8081:80
```

Then open <http://localhost:3000> for the frontend and <http://localhost:8081/todo> for the API gateway. The frontend defaults to the port-forwarded gateway URL when it is served from `localhost:3000`, which avoids browser calls to the un-forwarded `http://localhost/todo` endpoint.

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
kubectl delete -k k8s/overlays/local --ignore-not-found
minikube delete
```
