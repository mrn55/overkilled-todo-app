#!/usr/bin/env python3
"""Capture Phase 0 baseline latency and availability for /todo endpoints.

Usage examples:
  python3 scripts/phase0_baseline.py --base-url http://localhost --output docs/reports/phase0-baseline.json
  python3 scripts/phase0_baseline.py --base-url http://localhost --requests 30 --timeout 3
"""

from __future__ import annotations

import argparse
import json
import statistics
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone


def run_probe(method: str, url: str, timeout: float, payload: bytes | None = None) -> tuple[bool, int | None, float, str | None]:
    req = urllib.request.Request(url=url, method=method)
    if payload is not None:
        req.add_header("Content-Type", "application/json")

    started = time.perf_counter()
    try:
        with urllib.request.urlopen(req, data=payload, timeout=timeout) as resp:
            elapsed_ms = (time.perf_counter() - started) * 1000
            return True, resp.status, elapsed_ms, None
    except urllib.error.HTTPError as exc:
        elapsed_ms = (time.perf_counter() - started) * 1000
        return False, exc.code, elapsed_ms, str(exc)
    except Exception as exc:  # noqa: BLE001 - baseline tool should capture all failures.
        elapsed_ms = (time.perf_counter() - started) * 1000
        return False, None, elapsed_ms, str(exc)


def percentile(values: list[float], p: float) -> float:
    if not values:
        return 0.0
    ordered = sorted(values)
    idx = int(round((p / 100) * (len(ordered) - 1)))
    return ordered[idx]


def summarize(results: list[tuple[bool, int | None, float, str | None]]) -> dict:
    latencies = [r[2] for r in results]
    successes = [r for r in results if r[0]]
    failures = [r for r in results if not r[0]]

    return {
        "requests": len(results),
        "successes": len(successes),
        "failures": len(failures),
        "availability": round((len(successes) / len(results)) * 100, 3) if results else 0.0,
        "latency_ms": {
            "min": round(min(latencies), 3) if latencies else 0.0,
            "avg": round(statistics.fmean(latencies), 3) if latencies else 0.0,
            "p95": round(percentile(latencies, 95), 3),
            "p99": round(percentile(latencies, 99), 3),
            "max": round(max(latencies), 3) if latencies else 0.0,
        },
        "failure_samples": [
            {"status": status, "error": err} for _, status, _, err in failures[:5]
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Phase 0 /todo baseline probe")
    parser.add_argument("--base-url", default="http://localhost", help="Gateway base URL")
    parser.add_argument("--requests", type=int, default=25, help="Requests per probe")
    parser.add_argument("--timeout", type=float, default=2.0, help="Per-request timeout seconds")
    parser.add_argument("--output", default="docs/reports/phase0-baseline.json", help="Output JSON file path")
    args = parser.parse_args()

    target = f"{args.base_url.rstrip('/')}/todo"

    get_results = [run_probe("GET", target, args.timeout) for _ in range(args.requests)]

    now = datetime.now(timezone.utc).isoformat()
    report = {
        "captured_at_utc": now,
        "base_url": args.base_url,
        "target": target,
        "probes": {
            "GET /todo": summarize(get_results),
        },
        "notes": [
            "POST/PUT/DELETE baseline is intentionally excluded in Phase 0 to avoid mutating state during smoke runs.",
            "Add write-path probes in a controlled fixture environment before Phase 3.",
        ],
    }

    with open(args.output, "w", encoding="utf-8") as handle:
        json.dump(report, handle, indent=2)
        handle.write("\n")

    print(json.dumps(report, indent=2))
    print(f"\nWrote baseline report to: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
