# Integrate Istio

## 1. Have Istio CLI installed
Since I'm running Windows here I opted to grab the win release from: https://github.com/istio/istio/releases/tag/1.24.3
I have a folder at my root level with tools in it already in my `$PATH` so I dropped the binary there.

## 2. Install Istio onto cluster
```ps
# if the other ingress (nginx) is still live
kubectl delete -f k8s/ingress-manifest.yaml

istioctl install --set profile=demo -y

# look for life (or watch in k9s/lens)
kubectl get pods -n istio-system

kubectl apply -f .\istio\gateway.yaml
kubectl apply -f .\istio\virtualservice.yaml
kubectl apply -f .\istio\samples\addons
kubectl label namespace default istio-injection=enabled
kubectl apply -f .\istio\strict_mtls.yaml
kubectl rollout restart deploy

istioctl dashboard kiali
istioctl dashboard prometheus
```

## 3. Random
```
kubectl rollout restart deploy
```