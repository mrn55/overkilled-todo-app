# Architecture Overview

## Current Topology
- Frontend (`todo-frontend`) calls `http://localhost/todo`
- NGINX gateway routes by HTTP method:
  - `POST` -> create-service (Haskell)
  - `GET` -> read-service (Go)
  - `PUT` -> update-service (Rust)
  - `DELETE` -> delete-service (Erlang)
- All services connect to MariaDB

## Current Strengths
- Clean service boundaries by operation
- Polyglot implementation proving interoperability
- Local-first deployment with Docker Compose
- Kubernetes path already started for ingress/monitoring

## Target Architecture (After Upgrades)
1. **Contract Layer**
   - OpenAPI as source of truth
   - Generated clients and compatibility checks
2. **Telemetry Layer**
   - OTel traces, structured logs, RED metrics
3. **Correctness Layer**
   - Idempotency and optimistic concurrency
4. **Eventing Layer**
   - Outbox events + asynchronous projection
5. **Delivery Layer**
   - Canary rollout + rollback automation

## Request Flows
### Read Flow
Frontend -> Gateway -> Read Service -> DB -> Frontend

### Write Flow (Target)
Frontend -> Gateway -> CRUD Service -> DB transaction (state + outbox) -> Event dispatcher -> Projections

## Invariants
- API compatibility is checked before merge.
- Every production issue must be traceable with a request/trace id.
- Writes are safe under retries and concurrent edits.
