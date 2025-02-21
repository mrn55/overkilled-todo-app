# Stand app up in MiniKube

## (Prerequisite) Start Minikube
For me this was fairly simple, I'm using docker as the driver, I'm on windows, using powershell most the time.
```ps
minikube start
```
Enable ingress addon:
```ps
minikube addons enable ingress
```

## Get K8s Manifest
One option is to try convert `docker-compose.yml` file using `kompose convert`
See https://kubernetes.io/docs/tasks/configure-pod-container/translate-compose-kubernetes/
It produces a ton of files based on your docker-compose.yaml, more ideally you create a manifest file for each service you want to deploy.

## Add images to minikube
While we are using docker, minikube wont be able to see the images directly, so using the commands below we can ensure the manifest will deploy.
```ps
minikube image load overkilled-todo-app-create-service:latest
minikube image load overkilled-todo-app-read-service:latest
minikube image load overkilled-todo-app-update-service:latest
minikube image load overkilled-todo-app-delete-service:latest
minikube image load overkilled-todo-app-todo-frontend:latest
```

## Start deploying the manifests
```ps
kubectl apply -f k8s/database-manifest.yaml
kubectl apply -f k8s/create-service-manifest.yaml
kubectl apply -f k8s/read-service-manifest.yaml
kubectl apply -f k8s/update-service-manifest.yaml
kubectl apply -f k8s/delete-service-manifest.yaml
kubectl apply -f k8s/front-end-manifest.yaml
kubectl apply -f k8s/api-gateway-manifest.yaml
kubectl apply -f k8s/ingress-manifest.yaml
```

## Metrics!
https://grafana.com/blog/2023/01/19/how-to-monitor-kubernetes-clusters-with-the-prometheus-operator/
```ps
kubectl apply -f https://raw.githubusercontent.com/prometheus-operator/prometheus-operator/main/bundle.yaml --force-conflicts=true --server-side=true
kubectl apply -f .\k8s\prometheus_rbac.yaml
kubectl apply -f .\k8s\prometheus_instance.yaml
kubectl apply -f .\k8s\service_monitor.yaml
kubectl create deployment grafana --image=docker.io/grafana/grafana:latest 
kubectl apply -f .\k8s\expose_prometheus.yaml
kubectl expose deployment grafana --port 3000
kubectl port-forward svc/grafana 3000:3000
```
Follow the article linked just above to add Prometheus as a data source to Grafana.

## You can get in a container
Lets say you have a pod named `mariadb-686667c7dc-djc4d`.
```
kubectl exec --stdin --tty mariadb-686667c7dc-djc4d -- /bin/bash
```
Would get you in, using your credentials sored in the config map (base64 encoded in my deployment) you can log into MariaDB.
```
mysql -uroot -p
```

## Delete EVERYTHING
```ps
minikube delete
```
