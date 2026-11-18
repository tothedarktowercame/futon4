#!/usr/bin/env python3
"""Check the live ※ shortcode table against Circus.AnyKey.

This implements ``Table.hasLonger``, ``Table.costNoRet`` and
``Table.costRet`` from futon5 ``Circus/AnyKey.lean``.  Codes are converted to
typed keys by removing spaces because ※ inserts its own spaces.  Agency's
roster substitutes for Emacs' open-REPL buffers; this program never contacts
Emacs.
"""

from __future__ import annotations

import argparse
import json
import re
import urllib.request
from pathlib import Path
from typing import Any, Iterable


HERE = Path(__file__).resolve().parent
DEFAULT_TABLE = HERE / "mojira-codes.json"
DEFAULT_ALLOWLIST = Path.home() / "code/futon0/system/zone/code-allowlist/allowlist.tsv"
DEFAULT_AGENTS_URL = "http://localhost:7070/api/alpha/agents"


def typed_keys(code: str) -> str:
    """The typed-key sequence for CODE (prompt-inserted spaces cost nothing)."""
    return code.replace(" ", "")


def analyse_codes(codes: Iterable[str]) -> list[dict[str, Any]]:
    """Return the Lean costs and one proper extender for each distinct code."""
    unique = sorted(set(codes))
    typed = {code: typed_keys(code) for code in unique}
    rows: list[dict[str, Any]] = []
    for code in unique:
        key = typed[code]
        longer = next(
            (other for other in unique if typed[other] != key and typed[other].startswith(key)),
            None,
        )
        no_ret = len(key)
        rows.append(
            {
                "code": code,
                "typed": key,
                "costNoRet": no_ret,
                "costRet": no_ret + (1 if longer is not None else 0),
                "extends_to": longer,
            }
        )
    return rows


def repo_keys(allowlist: Path) -> list[str]:
    """Apply the exact key rule of ``mojira--repos`` to the allowlist."""
    keys: list[str] = []
    for line in allowlist.read_text(encoding="utf-8").splitlines():
        cols = line.split("\t")
        if len(cols) < 2 or cols[1] != "repo":
            continue
        name = cols[0]
        keys.append(f"r {name}")
        match = re.fullmatch(r"futon([0-9][0-9a-z]*)", name)
        if match:
            keys.append(match.group(1))
    return keys


def _agent_ids(payload: Any) -> list[str]:
    if isinstance(payload, dict) and "agents" in payload:
        payload = payload["agents"]
    if isinstance(payload, dict):
        return list(payload)
    if isinstance(payload, list):
        out = []
        for item in payload:
            if isinstance(item, str):
                out.append(item)
            elif isinstance(item, dict):
                ident = item.get("id") or item.get("name") or item.get("agent")
                if isinstance(ident, dict):
                    ident = ident.get("id/value")
                if isinstance(ident, str):
                    out.append(ident)
        return out
    raise ValueError("agents JSON must contain an agents object/list or be a list")


def agent_codes(payload: Any, agent_letters: dict[str, str], modal: bool) -> list[str]:
    """Convert Agency roster ids (KIND-N) to the Emacs letter-per-kind codes."""
    codes: list[str] = []
    kinds_seen: set[str] = set()
    for ident in _agent_ids(payload):
        match = re.fullmatch(r"([a-z]+)-([0-9]+)", ident)
        if not match or match.group(1) not in agent_letters:
            continue
        kind, number = match.groups()
        codes.append(f"{agent_letters[kind]} {number}")
        kinds_seen.add(kind)
    if modal:
        codes.extend(f"{agent_letters[kind]} l" for kind in sorted(kinds_seen))
    return codes


def read_agents(path: Path | None, url: str) -> tuple[Any, str]:
    if path is not None:
        return json.loads(path.read_text(encoding="utf-8")), f"Agency roster fixture {path}"
    with urllib.request.urlopen(url, timeout=5) as response:  # nosec: local read-only endpoint
        return json.load(response), f"Agency roster {url}"


def build_codes(table: dict[str, Any], allowlist: Path, agents: Any) -> list[str]:
    """Build precisely the families in ``mojira--all-codes``."""
    fixed = [entry["code"] for entry in table.get("codes", [])]
    kinds = [entry["key"] for entry in table.get("kinds", [])]
    modal = any(entry.get("key") == "l" for entry in table.get("agent_modals", []))
    letters = {entry["kind"]: entry["key"] for entry in table.get("agents", [])}
    codes = ["?", *fixed]
    if modal:
        codes.append("l")
    for repo in repo_keys(allowlist):
        codes.append(repo)
        if modal:
            codes.append(f"{repo} l")
        codes.extend(f"{repo} {kind}" for kind in kinds)
    codes.extend(agent_codes(agents, letters, modal))
    return sorted(set(codes))


def report(rows: list[dict[str, Any]], agent_source: str) -> dict[str, Any]:
    needs_ret = [row for row in rows if row["extends_to"] is not None]
    over = [row for row in rows if row["costRet"] > 5 or row["costNoRet"] > 5]
    return {
        "agent_source": agent_source,
        "agent_note": "Agency roster substitutes for Emacs open REPL buffers; Emacs was not contacted.",
        "code_count": len(rows),
        "needs_ret": needs_ret,
        "over_five": over,
        "anyKeyFive": "holds" if not over else f"fails ({len(over)} codes)",
    }


def text_report(result: dict[str, Any]) -> str:
    lines = [
        f"agents: {result['agent_source']}",
        f"note: {result['agent_note']}",
        f"codes: {result['code_count']}",
        "needs RET:",
    ]
    lines.extend(
        f"  {row['code']} <- {row['extends_to']} (costNoRet={row['costNoRet']}, costRet={row['costRet']})"
        for row in result["needs_ret"]
    )
    lines.append("over five:")
    lines.extend(
        f"  {row['code']} (costNoRet={row['costNoRet']}, costRet={row['costRet']})"
        for row in result["over_five"]
    )
    lines.append(f"anyKeyFive: {result['anyKeyFive']}")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--agents-json", type=Path)
    parser.add_argument("--agents-url", default=DEFAULT_AGENTS_URL)
    parser.add_argument("--codes-json", type=Path, default=DEFAULT_TABLE)
    parser.add_argument("--allowlist", type=Path, default=DEFAULT_ALLOWLIST)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    table = json.loads(args.codes_json.read_text(encoding="utf-8"))
    agents, source = read_agents(args.agents_json, args.agents_url)
    result = report(analyse_codes(build_codes(table, args.allowlist, agents)), source)
    print(json.dumps(result, indent=2, sort_keys=True) if args.json else text_report(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
