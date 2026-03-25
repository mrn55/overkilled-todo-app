# Phase 0 Execution Record

This document is the single source of truth for planning and completing Phase 0.

## Goal
Establish baseline architecture understanding, reliability targets, and measurable endpoint performance before introducing higher-order system complexity.

## Work Plan
1. Document current architecture and core invariants.
2. Define initial SLI/SLO targets and error budget policy.
3. Capture repeatable baseline metrics for `/todo` read path.
4. Create a standardized report template for future comparisons.

## Execution Checklist
- [x] Architecture snapshot and invariants documented (`docs/architecture.md`).
- [x] Initial SLO policy documented (`docs/slo.md`).
- [x] Baseline capture script added (`scripts/phase0_baseline.py`).
- [x] Baseline report template added (`docs/reports/phase0-baseline-template.md`).
- [x] Baseline run executed and raw JSON committed (`docs/reports/phase0-baseline.json`).
- [ ] Baseline run executed against a healthy live stack (current run was against an unavailable local gateway).
- [ ] Baseline markdown report committed as `docs/reports/phase0-baseline-YYYY-MM-DD.md`.

## Executed Command (2026-03-25 UTC)
```sh
python3 scripts/phase0_baseline.py --base-url http://localhost --requests 5 --timeout 1 --output docs/reports/phase0-baseline.json
```

## Current Findings
- Local gateway was unreachable during measurement (`connection refused`).
- This run validates tooling and report generation, not service performance.
- Next run should be performed after `docker-compose up --build -d`.

## How To Execute Healthy Baseline Run
1. Start stack:
   ```sh
   cp .env-example .env
   docker-compose up --build -d
   ```
2. Run probe:
   ```sh
   python3 scripts/phase0_baseline.py --base-url http://localhost --requests 30 --output docs/reports/phase0-baseline.json
   ```
3. Copy numbers into report template and commit both JSON + Markdown report.

## Exit Criteria (Phase 0)
- A new contributor can explain end-to-end read flow in under 2 minutes.
- SLO target values exist with a stated review cadence.
- Baseline report exists and can be regenerated from one command.
