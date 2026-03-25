# Game Day Runbook (Reliability Drills)

Run these drills after Phase 2 observability is in place.

## Objective
Validate the system's behavior during common failure modes and verify alerts/SLO indicators are meaningful.

## Prerequisites
- Stack running locally or in cluster
- Metrics dashboard available
- Trace visibility enabled

## Drill 1 — Read service outage
### Inject
- Stop/read-service instance or scale to zero.

### Expected
- `GET /todo` fails predictably.
- Errors are visible in dashboard and logs.
- Other operations remain unaffected as much as possible.

### Validate
- Error rate spikes are captured.
- Trace spans show failure boundary.
- Recovery time is recorded after service restoration.

## Drill 2 — DB latency injection
### Inject
- Add artificial delay in DB path or network latency.

### Expected
- Latency SLO degradation appears on p95 charts.
- Timeouts trigger as configured.
- Retries do not overwhelm dependent services.

### Validate
- Clear latency trend in metrics.
- No runaway retry storm.
- Error budget impact is measurable.

## Post-Drill Review Template
- Scenario:
- Start/stop times:
- SLO impact:
- Alerts fired (yes/no):
- Unexpected behavior:
- Action items:
