# Service Level Objectives (Initial)

These SLOs are intentionally simple and can tighten over time.

## SLI Definitions
- **Availability SLI**: successful HTTP responses / total requests
- **Latency SLI**: request duration distribution per endpoint
- **Correctness SLI**: percentage of write requests that preserve idempotency and concurrency guarantees

## SLO Targets (v1)
- `GET /todo`
  - Availability: 99.5%
  - p95 latency: < 250ms
- `POST|PUT|DELETE /todo*`
  - Availability: 99.0%
  - p95 latency: < 400ms

## Error Budget
- Monthly availability budget for 99.5%: ~3h 39m downtime
- Monthly availability budget for 99.0%: ~7h 18m downtime

## Alerting Strategy (Planned)
- Fast burn: 2% budget in 1 hour
- Slow burn: 5% budget in 6 hours

## Measurement Plan
- Metrics source: service instrumentation + gateway metrics
- Dashboards: per endpoint RED + error classes
- Review cadence: weekly reliability review

## Change Policy
Any change that can materially affect latency or error rates must include:
1. Expected SLO impact
2. Rollback strategy
3. Observability updates
