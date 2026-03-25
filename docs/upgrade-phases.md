# Overkill(ed) Todo App — Upgrade Phases

This document turns the roadmap into an execution plan that humans and agents can run incrementally.

## North Star
Build a **small app with big-system qualities**:
- Contract stability
- Deep observability
- Data correctness under concurrency
- Resilience under failure

## Scope Guardrails (No Bloat)
Only add work that creates one of these outcomes:
1. Better correctness
2. Better operability
3. Better reliability
4. Better security

If a task does not improve one of the above, defer it.

---

## Phase 0 — Baseline + Guardrails (1–2 days)

### Deliverables
- `docs/architecture.md`: current topology + request flows + invariants
- `docs/slo.md`: first SLO policy and error-budget posture
- `scripts/phase0_baseline.py`: reproducible latency/availability probe
- `docs/reports/phase0-baseline-template.md`: report format for comparable snapshots
- `docs/phase-0-execution.md`: execution checklist and status

### Execution Command
```sh
python3 scripts/phase0_baseline.py --base-url http://localhost --requests 30 --output docs/reports/phase0-baseline.json
```

### Exit Criteria
- A new contributor can explain request flow end-to-end in under 2 minutes.
- SLOs are measurable with existing telemetry or local scripts.
- A baseline report exists and is reproducible from one command.

---

## Phase 1 — Contract-First API (3–5 days)

### Deliverables
- `api/openapi/todo.v1.yaml`
- Generated TypeScript client under `todo-frontend/src/api/generated/`
- CI job for API lint + breaking-change detection

### Exit Criteria
- Frontend API calls use generated client.
- Contract checks fail CI on breaking changes.

---

## Phase 2 — End-to-End Observability (4–6 days)

### Deliverables
- Trace propagation from frontend -> gateway -> service -> DB
- Structured logs with trace IDs
- RED metrics dashboard (rate, errors, duration)

### Exit Criteria
- One user action is visible as one trace chain across services.
- p95 endpoint latency visible per service.

---

## Phase 3 — Correctness Semantics (4–7 days)

### Deliverables
- DB migration adding optimistic concurrency metadata (`version`)
- `PUT /todo/{id}` concurrency checks with conflict response
- Idempotency key support for `POST /todo`

### Exit Criteria
- Concurrent write conflict returns deterministic 409 behavior.
- Duplicate POST retries do not create duplicate records.

---

## Phase 4 — Outbox + Eventing (5–8 days)

### Deliverables
- Outbox schema migration
- Transactional write + outbox insert
- Dispatcher and replayable projector

### Exit Criteria
- No lost events between DB commit and publish.
- Read model can be rebuilt by replaying events.

---

## Phase 5 — Reliability + Progressive Delivery (3–5 days)

### Deliverables
- Timeout/retry/circuit policies
- Canary release for one service
- Two game-day scenarios documented and executed

### Exit Criteria
- Canary can auto-rollback on SLO breach.
- Failure drills are reproducible with runbook steps.

---

## Suggested Ticket Naming
Use this format to keep work searchable:
`[P{phase}] {area}: {outcome}`

Examples:
- `[P1] contract: add OpenAPI schema for todo v1`
- `[P2] telemetry: propagate traceparent through gateway`
- `[P3] data: enforce optimistic concurrency on update`

## Definition of Done (All Phases)
- Docs updated
- Tests/checks pass locally
- Observability added for new critical paths
- Rollback plan documented for risky changes
