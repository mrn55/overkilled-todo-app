# Phase 0 Baseline Report

- Date (UTC): `YYYY-MM-DD`
- Environment: `local docker-compose | minikube | other`
- Git commit: `<sha>`
- Command:
  ```sh
  python3 scripts/phase0_baseline.py --base-url http://localhost --requests 30 --output docs/reports/phase0-baseline.json
  ```

## Summary
- GET availability: `xx.xxx%`
- GET p95 latency: `xxx ms`
- GET p99 latency: `xxx ms`
- SLO pass/fail: `PASS | FAIL`

## Raw Output
Attach or reference:
- `docs/reports/phase0-baseline.json`

## Observations
- Example: warm-up effects observed in first 3 requests.
- Example: failures tied to gateway startup race.

## Follow-Ups
- [ ] Tune timeout values
- [ ] Add write-path controlled benchmark fixture
- [ ] Compare against previous run and document trend
