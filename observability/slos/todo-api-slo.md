# TODO API SLO

## User Journey

Users can load the frontend and use the `/todo` API through the AKS ingress.

## Service Level Objective

For the dev portfolio environment, 99% of `/todo` ingress requests should complete without a 5xx response over a rolling 7-day window.

## Service Level Indicator

Good events:

```promql
sum(rate(nginx_ingress_controller_requests{namespace="todo-app",status!~"5.."}[5m]))
```

Total events:

```promql
sum(rate(nginx_ingress_controller_requests{namespace="todo-app"}[5m]))
```

Availability ratio:

```promql
sum(rate(nginx_ingress_controller_requests{namespace="todo-app",status!~"5.."}[5m]))
/
clamp_min(sum(rate(nginx_ingress_controller_requests{namespace="todo-app"}[5m])), 0.001)
```

## Error Budget

A 99% SLO allows 1% of ingress requests to fail with 5xx responses during the window. In a 10,000-request demo window, the budget is 100 failed requests.

Burning the budget means the next work should prioritize reliability fixes over new platform features. For this portfolio environment, budget burn is investigated with the high-error-rate and database-unavailable runbooks before changing application behavior.
