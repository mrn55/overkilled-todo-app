# Agent Execution Playbook

This playbook helps human contributors and coding agents execute upgrades consistently.

## Execution Rules
1. Work in one phase at a time.
2. Keep pull requests small and reversible.
3. Add/adjust observability whenever behavior changes.
4. Prefer contract-first changes for any API modification.
5. Before starting Phase 1+, ensure Phase 0 checklist is complete in `docs/phase-0-execution.md`.

## PR Checklist
- [ ] Scope linked to one phase (`docs/upgrade-phases.md`)
- [ ] Docs updated (architecture/SLO/runbook if applicable)
- [ ] Tests/checks run and captured in PR
- [ ] Rollback notes included
- [ ] No unrelated refactors

## Branching Convention
- `phase-<n>/<short-topic>`
Examples:
- `phase-1/openapi-contract`
- `phase-2/otel-tracing`

## Commit Message Convention
`[P<n>] <area>: <summary>`

Examples:
- `[P1] contract: add todo v1 OpenAPI schema`
- `[P3] data: enforce optimistic concurrency on PUT`

## Task Slicing Heuristic
A task is correctly sized if it can be:
- reviewed in under 20 minutes,
- rolled back with one revert commit,
- validated with one clear acceptance check.

## Risk Tiers
- **Low**: docs, tests, non-runtime behavior
- **Medium**: endpoint handlers, schema-compatible changes
- **High**: DB migrations, gateway routing, auth, reliability policies

High-risk changes must include:
- explicit rollback steps,
- before/after metrics plan,
- at least one failure-mode test.
