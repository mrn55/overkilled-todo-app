# ADR 0001: Keep the business app simple

## Status

Accepted

## Context

The repository exists as a platform engineering portfolio project. Adding richer TODO product features would make the app more interesting as a product, but it would distract from the intended evaluation criteria: repeatable development, container hygiene, Kubernetes promotion paths, Azure infrastructure, GitOps, observability, and security controls.

## Decision

Keep TODO CRUD behavior intentionally simple. New work should primarily improve the platform around the app unless a user explicitly asks for business functionality.

## Consequences

- Reviewers can understand the app quickly and spend their attention on platform design.
- Demo scripts can focus on operational maturity instead of product complexity.
- Future milestones should avoid product scope creep and map work back to `docs/platform-milestones.md`.
