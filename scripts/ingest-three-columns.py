#!/usr/bin/env python3
"""Ingest three-column data into futon1a hyperedge store.

Columns:
  1. Math (from futon6 JSON hypergraph files)
  2. Code (from futon3c reflection API)
  3. Project (from futon3c mission-control API)

Usage:
  python3 scripts/ingest-three-columns.py [--math] [--code] [--project] [--all]
  python3 scripts/ingest-three-columns.py --count   # just report current count
"""

import argparse
import json
import os
import sys
import urllib.request
import urllib.error
from pathlib import Path

FUTON1A = "http://localhost:7071"
FUTON3C = "http://localhost:7070"
PENHOLDER = "api"

# --- HTTP helpers ---

def post_hyperedge(hx_type, endpoints, props=None, content=None, labels=None):
    """POST a single hyperedge to futon1a. Returns the response dict or None on error."""
    payload = {
        "hx/type": hx_type,
        "hx/endpoints": endpoints,
    }
    if props:
        payload["hx/props"] = props
    if content:
        payload["hx/content"] = content
    if labels:
        payload["hx/labels"] = labels

    data = json.dumps(payload).encode()
    req = urllib.request.Request(
        f"{FUTON1A}/api/alpha/hyperedge",
        data=data,
        headers={
            "Content-Type": "application/json",
            "X-Penholder": PENHOLDER,
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req) as resp:
            return json.loads(resp.read())
    except urllib.error.HTTPError as e:
        body = e.read().decode()
        print(f"  ERROR writing {hx_type} {endpoints[:2]}...: {e.code} {body}", file=sys.stderr)
        return None


def get_json(url):
    """GET JSON from a URL."""
    req = urllib.request.Request(url)
    with urllib.request.urlopen(req) as resp:
        return json.loads(resp.read())


def query_hyperedge_count(hx_type):
    """Query hyperedge count by type from futon1a. Returns int."""
    import re
    url = f"{FUTON1A}/api/alpha/hyperedges?type={hx_type}&limit=1000"
    try:
        req = urllib.request.Request(url)
        with urllib.request.urlopen(req) as resp:
            body = resp.read().decode()
            # futon1a returns EDN — extract :count N
            m = re.search(r':count\s+(\d+)', body)
            return int(m.group(1)) if m else 0
    except urllib.error.HTTPError:
        return 0


# --- Math Column ---

def ingest_math(json_path):
    """Ingest math hypergraph JSON into futon1a.

    Strategy:
    - Each node becomes a hyperedge of type 'math/{node-type}' with a single
      endpoint (the node ID). This registers the entity in the store.
    - Each edge becomes a hyperedge of type 'math/{edge-type}' with endpoints
      matching the edge's ends array.
    """
    print(f"\n=== MATH COLUMN: {json_path} ===")

    with open(json_path) as f:
        data = json.load(f)

    thread_id = data.get("thread_id", "unknown")
    nodes = data.get("nodes", [])
    edges = data.get("edges", [])
    print(f"  Thread {thread_id}: {len(nodes)} nodes, {len(edges)} edges")

    written = 0
    errors = 0

    # Phase 1: Nodes as entity-registration hyperedges
    for node in nodes:
        nid = node["id"]
        ntype = node.get("type", "unknown")
        subtype = node.get("subtype")
        attrs = node.get("attrs", {})

        hx_type = f"math/{ntype}"
        props = {"subtype": subtype} if subtype else {}
        if "score" in attrs:
            props["score"] = attrs["score"]
        if "is_accepted" in attrs:
            props["is_accepted"] = attrs["is_accepted"]

        # Content: store the full attrs for scope/expression nodes
        content = None
        if ntype in ("scope", "expression") and attrs:
            content = attrs

        # Single-endpoint hyperedge registers the entity
        result = post_hyperedge(hx_type, [nid], props=props or None, content=content)
        if result:
            written += 1
        else:
            errors += 1

    # Phase 2: Edges as relational hyperedges
    for edge in edges:
        etype = edge.get("type", "unknown")
        ends = edge.get("ends", [])
        attrs = edge.get("attrs", {})

        if not ends:
            continue

        hx_type = f"math/{etype}"
        props = {}
        if "act" in attrs:
            props["act"] = attrs["act"]
        if "position" in attrs:
            props["position"] = attrs["position"]

        # Store remaining attrs as content
        content = {k: v for k, v in attrs.items() if k not in ("act", "position")} or None

        result = post_hyperedge(hx_type, ends, props=props or None, content=content)
        if result:
            written += 1
        else:
            errors += 1

    print(f"  Written: {written}, Errors: {errors}")
    return written, errors


def ingest_math_column():
    """Ingest all available math JSON files."""
    total_written = 0
    total_errors = 0

    # Primary: thread-633512
    primary = Path("/home/joe/code/futon6/data/first-proof/thread-633512-hypergraph.json")
    if primary.exists():
        w, e = ingest_math(primary)
        total_written += w
        total_errors += e

    # Additional showcase files
    showcase_dir = Path("/home/joe/code/futon6/data/showcases")
    if showcase_dir.exists():
        for f in sorted(showcase_dir.glob("*-hypergraph.json")):
            w, e = ingest_math(f)
            total_written += w
            total_errors += e

    # CT validation wires
    wires = Path("/home/joe/code/futon6/data/ct-validation/wires.json")
    if wires.exists():
        print(f"\n=== MATH COLUMN (wires): {wires} ===")
        with open(wires) as f:
            wire_data = json.load(f)
        if isinstance(wire_data, list):
            w = 0
            for wire in wire_data:
                hx_type = wire.get("hx/type", "math/wire")
                endpoints = wire.get("hx/endpoints", [])
                content = wire.get("hx/content")
                if endpoints:
                    result = post_hyperedge(hx_type, endpoints, content=content)
                    if result:
                        w += 1
            total_written += w
            print(f"  Written: {w}")

    print(f"\n  MATH TOTAL: {total_written} written, {total_errors} errors")
    return total_written


# --- Code Column ---

def ingest_code_column():
    """Ingest code column from futon3c reflection API.

    Entity types:
    - code/namespace: each loaded namespace
    - code/var: each public var in futon3c namespaces
    - code/requires: namespace dependency edge
    """
    print("\n=== CODE COLUMN ===")

    # Get all namespaces, filter to futon-related
    resp = get_json(f"{FUTON3C}/api/alpha/reflect/namespaces")
    all_ns = resp.get("namespaces", [])

    futon_prefixes = ("futon", "agency", "arxana")
    futon_ns = [
        ns for ns in all_ns
        if any(ns["ns"].startswith(p) for p in futon_prefixes)
    ]
    print(f"  Total namespaces: {len(all_ns)}, futon-related: {len(futon_ns)}")

    written = 0
    errors = 0

    # Phase 1: Namespace entities
    for ns_info in futon_ns:
        ns_name = ns_info["ns"]
        props = {}
        if ns_info.get("doc"):
            props["doc"] = ns_info["doc"][:200]  # truncate long docs
        if ns_info.get("file"):
            props["file"] = ns_info["file"]

        result = post_hyperedge("code/namespace", [f"ns:{ns_name}"], props=props or None)
        if result:
            written += 1
        else:
            errors += 1

    # Phase 2: Vars for key namespaces (sample — not all, to avoid flooding)
    key_namespaces = [
        ns["ns"] for ns in futon_ns
        if any(ns["ns"].startswith(p) for p in (
            "futon3c.peripheral.",
            "futon3c.reflection.",
            "futon3c.logic.",
            "futon3c.portfolio.",
            "futon3c.transport.",
            "futon1a.api.",
            "futon1a.core.",
            "futon1a.compat.",
        ))
    ]
    print(f"  Key namespaces for var snapshot: {len(key_namespaces)}")

    var_count = 0
    for ns_name in key_namespaces:
        try:
            ns_resp = get_json(f"{FUTON3C}/api/alpha/reflect/ns/{ns_name}")
            vars_list = ns_resp.get("vars", [])
        except Exception as e:
            print(f"  WARN: could not reflect {ns_name}: {e}", file=sys.stderr)
            continue

        for var_info in vars_list:
            var_name = var_info.get("name", "?")
            var_id = f"var:{ns_name}/{var_name}"
            props = {}
            if var_info.get("doc"):
                props["doc"] = var_info["doc"][:200]
            if var_info.get("arglists"):
                props["arglists"] = str(var_info["arglists"])
            if var_info.get("line"):
                props["line"] = var_info["line"]
            if var_info.get("file"):
                props["file"] = var_info["file"]

            # var entity
            result = post_hyperedge("code/var", [var_id], props=props or None)
            if result:
                written += 1
                var_count += 1
            else:
                errors += 1

            # ns-contains-var relation
            result = post_hyperedge(
                "code/ns-contains-var",
                [f"ns:{ns_name}", var_id],
            )
            if result:
                written += 1
            else:
                errors += 1

    print(f"  Vars snapshotted: {var_count}")

    # Phase 3: Namespace dependencies
    dep_count = 0
    for ns_info in futon_ns:
        ns_name = ns_info["ns"]
        try:
            deps_resp = get_json(f"{FUTON3C}/api/alpha/reflect/deps/{ns_name}")
            deps = deps_resp.get("deps", {})
            requires = deps.get("requires", [])
        except Exception:
            continue

        for req_ns in requires:
            # Only track deps within futon ecosystem
            if any(req_ns.startswith(p) for p in futon_prefixes):
                result = post_hyperedge(
                    "code/requires",
                    [f"ns:{ns_name}", f"ns:{req_ns}"],
                )
                if result:
                    written += 1
                    dep_count += 1
                else:
                    errors += 1

    print(f"  Dependency edges: {dep_count}")
    print(f"  CODE TOTAL: {written} written, {errors} errors")
    return written


# --- Project Column ---

def ingest_project_column():
    """Ingest project column from futon3c mission-control API.

    Entity types:
    - project/devmap: each devmap
    - project/component: each component in a devmap
    - project/tension: each detected tension
    - project/trace-path: each trace path from the trace API
    """
    print("\n=== PROJECT COLUMN ===")

    written = 0
    errors = 0

    # Phase 1: Devmaps and components
    devmaps_resp = get_json(f"{FUTON3C}/api/alpha/mc/devmaps")
    devmaps = devmaps_resp.get("devmaps", [])
    print(f"  Devmaps: {len(devmaps)}")

    for dm in devmaps:
        dm_id = dm["devmap/id"]
        props = {
            "component-count": dm.get("devmap/component-count", 0),
            "state": dm.get("devmap/state", "unknown"),
        }
        if dm.get("devmap/edge-count"):
            props["edge-count"] = dm["devmap/edge-count"]

        result = post_hyperedge("project/devmap", [f"dm:{dm_id}"], props=props)
        if result:
            written += 1
        else:
            errors += 1

        # Components
        for comp in dm.get("devmap/components", []):
            comp_id = comp if isinstance(comp, str) else comp.get("id", str(comp))
            comp_full_id = f"comp:{dm_id}/{comp_id}"

            result = post_hyperedge("project/component", [comp_full_id], props={"devmap": dm_id})
            if result:
                written += 1
            else:
                errors += 1

            # devmap-contains-component
            result = post_hyperedge(
                "project/devmap-contains",
                [f"dm:{dm_id}", comp_full_id],
            )
            if result:
                written += 1
            else:
                errors += 1

    # Phase 2: Tensions
    tensions_resp = get_json(f"{FUTON3C}/api/alpha/mc/tensions")
    tensions = tensions_resp.get("tensions", [])
    print(f"  Tensions: {len(tensions)}")

    for t in tensions:
        t_type = t.get("tension/type", "unknown")
        t_devmap = t.get("tension/devmap", "")
        t_component = t.get("tension/component", "")
        t_id = f"tension:{t_devmap}/{t_component}"

        props = {
            "type": t_type,
            "summary": t.get("tension/summary", ""),
        }
        if t.get("tension/coverage-pct") is not None:
            props["coverage-pct"] = t["tension/coverage-pct"]

        # Tension as entity
        result = post_hyperedge("project/tension", [t_id], props=props)
        if result:
            written += 1
        else:
            errors += 1

        # Link tension to component
        comp_full_id = f"comp:{t_devmap}/{t_component}"
        result = post_hyperedge(
            "project/tension-on",
            [t_id, comp_full_id],
        )
        if result:
            written += 1
        else:
            errors += 1

    # Phase 3: Trace paths
    trace_resp = get_json(f"{FUTON3C}/api/alpha/mc/trace")
    paths = trace_resp.get("paths", [])
    print(f"  Trace paths: {len(paths)}")

    for i, path in enumerate(paths):
        path_id = f"trace:{i}"
        devmap = path.get("tension/devmap", "")
        component = path.get("tension/component", "")

        gates = path.get("gates", [])
        gate_summary = []
        for g in gates:
            gate_summary.append({
                "gate": g.get("gate/name", "?"),
                "status": g.get("gate/status", "?"),
            })

        props = {
            "devmap": devmap,
            "component": component,
            "gate-count": len(gates),
        }
        content = {"gates": gate_summary}

        result = post_hyperedge("project/trace-path", [path_id], props=props, content=content)
        if result:
            written += 1
        else:
            errors += 1

    print(f"  PROJECT TOTAL: {written} written, {errors} errors")
    return written


# --- Cross-Column Invariants ---

def query_all_of_type(hx_type, limit=2000):
    """Return all hyperedges of a given type as a list of dicts (parsed from EDN)."""
    import re
    url = f"{FUTON1A}/api/alpha/hyperedges?type={hx_type}&limit={limit}"
    try:
        req = urllib.request.Request(url)
        with urllib.request.urlopen(req) as resp:
            body = resp.read().decode()
            # Minimal EDN parsing: extract hx/id values and props
            # For invariant checking we mainly need endpoint IDs and props
            results = []
            # Split on hx/id boundaries
            for chunk in body.split(":hx/id ")[1:]:
                hx_id_match = re.match(r'"([^"]+)"', chunk)
                if hx_id_match:
                    hx_id = hx_id_match.group(1)
                    # Extract endpoints
                    ep_match = re.search(r':hx/endpoints \[([^\]]*)\]', chunk)
                    endpoints = []
                    if ep_match:
                        endpoints = re.findall(r'"([^"]+)"', ep_match.group(1))
                    # Check for :doc prop
                    has_doc = ':doc ' in chunk and ':doc nil' not in chunk
                    results.append({
                        "hx/id": hx_id,
                        "hx/endpoints": endpoints,
                        "has_doc": has_doc,
                        "_raw": chunk[:500],
                    })
            return results
    except urllib.error.HTTPError:
        return []


def check_invariants():
    """Run cross-column invariant checks and emit violations as hyperedges.

    INV-1: Undocumented Entry Points (Project↔Code)
      Namespaces that exist in the code column but have no :doc prop.

    INV-2: Uncovered Components (Project only, but cross-column ready)
      Components with active tensions (already detected by MC).
      Stored as invariant violations for browsability.

    INV-3: Orphan Namespaces (Code↔Code)
      Namespaces with no inbound requires edge — potential dead code
      or missing integration.
    """
    print("\n=== CROSS-COLUMN INVARIANT CHECKS ===")

    violations = []

    # INV-1: Undocumented Entry Points
    print("\n  INV-1: Undocumented Entry Points (Project↔Code)")
    namespaces = query_all_of_type("code/namespace")
    undocumented = [ns for ns in namespaces if not ns["has_doc"]]
    print(f"    Total namespaces: {len(namespaces)}, undocumented: {len(undocumented)}")

    for ns in undocumented:
        ns_id = ns["hx/endpoints"][0] if ns["hx/endpoints"] else "?"
        violations.append({
            "invariant": "INV-1",
            "type": "undocumented-entry-point",
            "entity": ns_id,
            "summary": f"Namespace {ns_id} has no docstring",
            "resolution": "Add ns docstring or mark as internal",
        })

    # INV-2: Uncovered Components
    print("\n  INV-2: Uncovered Components (Project)")
    tensions = query_all_of_type("project/tension")
    print(f"    Active tensions: {len(tensions)}")

    for t in tensions:
        t_id = t["hx/endpoints"][0] if t["hx/endpoints"] else "?"
        violations.append({
            "invariant": "INV-2",
            "type": "uncovered-component",
            "entity": t_id,
            "summary": f"Component {t_id} has no covering mission",
            "resolution": "Create a mission or annotate coverage",
        })

    # INV-3: Orphan Namespaces
    print("\n  INV-3: Orphan Namespaces (Code↔Code)")
    requires_edges = query_all_of_type("code/requires")
    # Build set of namespaces that are required by at least one other
    required_ns = set()
    for edge in requires_edges:
        if len(edge["hx/endpoints"]) >= 2:
            required_ns.add(edge["hx/endpoints"][1])  # target is the required ns

    all_ns_ids = {ns["hx/endpoints"][0] for ns in namespaces if ns["hx/endpoints"]}
    orphans = all_ns_ids - required_ns
    # Filter out top-level entry points (dev, main, system namespaces)
    entry_point_patterns = ("futon3c.dev", "futon3c.system", "futon1a.system", "futon1a.dev")
    orphans = {
        ns for ns in orphans
        if not any(ns.replace("ns:", "").startswith(p) for p in entry_point_patterns)
    }
    print(f"    Total ns: {len(all_ns_ids)}, required by others: {len(required_ns)}, orphans: {len(orphans)}")

    for ns_id in sorted(orphans):
        violations.append({
            "invariant": "INV-3",
            "type": "orphan-namespace",
            "entity": ns_id,
            "summary": f"Namespace {ns_id} has no inbound requires edge",
            "resolution": "Verify namespace is reachable or remove",
        })

    # INV-4: Ungrounded Definitions (Math↔Math)
    print("\n  INV-4: Ungrounded Definitions (Math↔Math)")
    scopes = query_all_of_type("math/scope")
    # Get all scope node IDs (single-endpoint = entity registration)
    scope_ids = set()
    for s in scopes:
        eps = s["hx/endpoints"]
        if len(eps) == 1:
            scope_ids.add(eps[0])

    # Check which scopes appear in iatc edges (referenced in argumentation)
    iatc_edges = query_all_of_type("math/iatc")
    referenced_in_iatc = set()
    for edge in iatc_edges:
        for ep in edge["hx/endpoints"]:
            if ep in scope_ids:
                referenced_in_iatc.add(ep)

    # Also check scope-type relational edges (scope containment)
    scope_edges = [s for s in scopes if len(s["hx/endpoints"]) == 2]
    in_scope_relation = set()
    for edge in scope_edges:
        for ep in edge["hx/endpoints"]:
            if ep in scope_ids:
                in_scope_relation.add(ep)

    # Ungrounded = in no iatc edge AND not contained in another scope
    ungrounded = scope_ids - referenced_in_iatc
    print(f"    Total scopes: {len(scope_ids)}, in iatc: {len(referenced_in_iatc)}, "
          f"in scope relation: {len(in_scope_relation)}, ungrounded: {len(ungrounded)}")

    for sid in sorted(ungrounded):
        violations.append({
            "invariant": "INV-4",
            "type": "ungrounded-definition",
            "entity": sid,
            "summary": f"Scope {sid} is never referenced in argumentation (no iatc edge)",
            "resolution": "Add iatc edge (assert/clarify/reference) linking this definition to a post",
        })

    # Emit violations as hyperedges
    print(f"\n  Total violations: {len(violations)}")
    print("  Writing violation hyperedges to futon1a...")

    written = 0
    for v in violations:
        result = post_hyperedge(
            f"invariant/{v['type']}",
            [v["entity"]],
            props={
                "invariant": v["invariant"],
                "summary": v["summary"],
                "resolution": v["resolution"],
            },
            labels=["invariant-violation"],
        )
        if result:
            written += 1

    print(f"  Violations written: {written}")

    # Summary
    print("\n  VIOLATION SUMMARY:")
    from collections import Counter
    by_inv = Counter(v["invariant"] for v in violations)
    for inv, count in sorted(by_inv.items()):
        print(f"    {inv}: {count} violations")

    return violations


# --- Enrichment Layer 0 ---

REPOS = {
    "futon3c": Path("/home/joe/code/futon3c"),
    "futon1a": Path("/home/joe/code/futon1a"),
    "futon4": Path("/home/joe/code/futon4"),
    "futon3": Path("/home/joe/code/futon3"),
    "futon5": Path("/home/joe/code/futon5"),
}

FILE_EXTS = (".clj", ".cljc", ".cljs", ".el", ".py", ".edn")


def ingest_file_churn(since_days=90):
    """Ingest git commit counts per file across futon repos."""
    print("\n=== ENRICHMENT L0: FILE CHURN ===")
    import subprocess
    from datetime import datetime, timedelta

    since_date = (datetime.now() - timedelta(days=since_days)).strftime("%Y-%m-%d")
    written = 0
    errors = 0

    for repo_name, repo_path in REPOS.items():
        if not (repo_path / ".git").exists():
            continue

        # All-time churn
        try:
            out = subprocess.check_output(
                ["git", "log", "--format=", "--name-only"],
                cwd=repo_path, text=True, stderr=subprocess.DEVNULL,
            )
        except subprocess.CalledProcessError:
            continue

        all_time = {}
        for line in out.strip().split("\n"):
            line = line.strip()
            if line and any(line.endswith(ext) for ext in FILE_EXTS):
                all_time[line] = all_time.get(line, 0) + 1

        # Recent churn
        try:
            out = subprocess.check_output(
                ["git", "log", f"--since={since_date}", "--format=", "--name-only"],
                cwd=repo_path, text=True, stderr=subprocess.DEVNULL,
            )
        except subprocess.CalledProcessError:
            out = ""

        recent = {}
        for line in out.strip().split("\n"):
            line = line.strip()
            if line and any(line.endswith(ext) for ext in FILE_EXTS):
                recent[line] = recent.get(line, 0) + 1

        # Last-touch dates
        for filepath, count in all_time.items():
            full = repo_path / filepath
            if not full.exists():
                continue

            try:
                touch = subprocess.check_output(
                    ["git", "log", "-1", "--format=%Y-%m-%d", "--", filepath],
                    cwd=repo_path, text=True, stderr=subprocess.DEVNULL,
                ).strip()
            except subprocess.CalledProcessError:
                touch = ""

            props = {
                "repo": repo_name,
                "commits-all-time": count,
                "commits-recent": recent.get(filepath, 0),
                "last-touch": touch,
            }
            result = post_hyperedge(
                "code/file-churn",
                [f"file:{repo_name}/{filepath}"],
                props=props,
            )
            if result:
                written += 1
            else:
                errors += 1

        print(f"  {repo_name}: {len(all_time)} files with churn data")

    print(f"  CHURN TOTAL: {written} written, {errors} errors")
    return written


def ingest_indentation_complexity():
    """Ingest indentation-based complexity metrics per file."""
    print("\n=== ENRICHMENT L0: INDENTATION COMPLEXITY ===")

    written = 0
    errors = 0

    for repo_name, repo_path in REPOS.items():
        if not repo_path.exists():
            continue

        count = 0
        for root, dirs, files in os.walk(repo_path / "src"):
            _scan_dir(root, files, repo_name, repo_path)
        for root, dirs, files in os.walk(repo_path / "dev"):
            _scan_dir(root, files, repo_name, repo_path)
        # Also scan scripts/ and test/ but not node_modules etc
        for subdir in ("test", "scripts"):
            target = repo_path / subdir
            if target.exists():
                for root, dirs, files in os.walk(target):
                    _scan_dir(root, files, repo_name, repo_path)

        # Collect results from the helper
        for filepath, props in _complexity_results:
            result = post_hyperedge(
                "code/indentation-complexity",
                [f"file:{repo_name}/{filepath}"],
                props=props,
            )
            if result:
                written += 1
                count += 1
            else:
                errors += 1

        print(f"  {repo_name}: {count} files with complexity data")
        _complexity_results.clear()

    print(f"  COMPLEXITY TOTAL: {written} written, {errors} errors")
    return written


_complexity_results = []


def _scan_dir(root, files, repo_name, repo_path):
    """Scan files in a directory for indentation complexity."""
    for f in files:
        if not any(f.endswith(ext) for ext in FILE_EXTS):
            continue
        full = os.path.join(root, f)
        try:
            with open(full) as fh:
                lines = fh.readlines()
        except (OSError, UnicodeDecodeError):
            continue
        if not lines:
            continue
        depths = [len(l) - len(l.lstrip()) for l in lines if l.strip()]
        if not depths:
            continue
        rel = os.path.relpath(full, repo_path)
        _complexity_results.append((rel, {
            "repo": repo_name,
            "max-depth": max(depths),
            "mean-depth": round(sum(depths) / len(depths), 1),
            "lines": len(lines),
        }))


def record_enrichment_layer(layer_num, before_total, after_total, anomalies=None):
    """Record a meta/enrichment-layer hyperedge."""
    from datetime import datetime

    props = {
        "layer": layer_num,
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "before-total": before_total,
        "after-total": after_total,
        "delta": after_total - before_total,
        "mission": "M-futon-enrichment",
    }
    if anomalies:
        props["anomalies"] = anomalies

    result = post_hyperedge(
        "meta/enrichment-layer",
        [f"layer:{layer_num}"],
        props=props,
        labels=["enrichment", f"layer-{layer_num}"],
    )
    if result:
        print(f"\n  Layer {layer_num} metadata recorded: +{after_total - before_total} hyperedges")
    else:
        print(f"\n  WARNING: failed to record layer {layer_num} metadata")
    return result


def run_layer0():
    """Run enrichment Layer 0: topology + churn + complexity + invariants."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 0: Namespace Topology + Behavioral Signals")
    print("=" * 60)

    # Count before
    before = count_hyperedges()

    total = 0

    # Existing code column (namespaces, vars, requires)
    total += ingest_code_column()

    # New: file churn
    total += ingest_file_churn()

    # New: indentation complexity
    total += ingest_indentation_complexity()

    # Existing: cross-column invariants
    check_invariants()

    # Count after
    after = count_hyperedges()

    # Hotspot ranking
    print("\n=== HOTSPOT RANKING (churn x max-depth, top 10) ===")
    # We can't easily cross-reference from XTDB here, so compute locally
    import subprocess
    hotspots = []
    for repo_name, repo_path in REPOS.items():
        if not (repo_path / ".git").exists():
            continue
        try:
            out = subprocess.check_output(
                ["git", "log", "--since=2025-12-01", "--format=", "--name-only"],
                cwd=repo_path, text=True, stderr=subprocess.DEVNULL,
            )
        except subprocess.CalledProcessError:
            continue
        churn = {}
        for line in out.strip().split("\n"):
            line = line.strip()
            if line:
                churn[line] = churn.get(line, 0) + 1
        for root, dirs, files in os.walk(repo_path / "src"):
            for f in files:
                if not any(f.endswith(ext) for ext in FILE_EXTS):
                    continue
                full = os.path.join(root, f)
                rel = os.path.relpath(full, repo_path)
                ch = churn.get(rel, 0)
                if ch == 0:
                    continue
                try:
                    with open(full) as fh:
                        lines = fh.readlines()
                    depths = [len(l) - len(l.lstrip()) for l in lines if l.strip()]
                    if depths:
                        hotspots.append((ch * max(depths), ch, max(depths), f"{repo_name}/{rel}"))
                except (OSError, UnicodeDecodeError):
                    pass

    hotspots.sort(reverse=True)
    anomalies = []
    for score, ch, cx, p in hotspots[:10]:
        print(f"  score={score:5d}  churn={ch:2d}  depth={cx:2d}  {p}")
        if score > 1000:
            anomalies.append(f"hotspot: {p} (score={score})")

    # Record layer metadata
    record_enrichment_layer(0, before, after, anomalies=anomalies or None)

    print(f"\n=== LAYER 0 COMPLETE: {total} hyperedges written ===")
    return total


# --- Enrichment Layer 1: Mission Provenance ---

MISSION_DIRS = {
    "futon3c": Path("/home/joe/code/futon3c/holes/missions"),
    "futon4": Path("/home/joe/code/futon4/holes/missions"),
    "futon3": Path("/home/joe/code/futon3/holes/missions"),
}


def parse_mission_file(path):
    """Parse an M-*.md file for status, :out files, and file references."""
    import re

    mission_id = path.stem  # e.g. M-agency-refactor
    status = None
    out_files = []
    file_refs = []

    try:
        text = path.read_text()
    except (OSError, UnicodeDecodeError):
        return None

    # Extract status
    m = re.search(r'\*\*Status:\*\*\s*(\S+)', text)
    if m:
        status = m.group(1).rstrip(",")

    # Extract :out file blocks — lines like `:out — path/to/file.clj`
    for m in re.finditer(r':out\s*[—–-]\s*(.+)', text):
        out_files.append(m.group(1).strip().strip('`'))

    # Extract backtick-quoted file paths (src/..., test/..., dev/...)
    for m in re.finditer(r'`((?:src|test|dev|scripts)/[^\s`]+\.(?:clj|cljc|cljs|el|py|edn))`', text):
        file_refs.append(m.group(1))

    # Deduplicate
    all_files = list(dict.fromkeys(out_files + file_refs))

    return {
        "mission_id": mission_id,
        "status": status,
        "path": str(path),
        "out_files": out_files,
        "file_refs": file_refs,
        "all_files": all_files,
    }


def scan_all_missions():
    """Scan all mission dirs, return list of parsed missions."""
    missions = []
    for repo_name, mission_dir in MISSION_DIRS.items():
        if not mission_dir.exists():
            continue
        for md_file in sorted(mission_dir.glob("M-*.md")):
            parsed = parse_mission_file(md_file)
            if parsed:
                parsed["repo"] = repo_name
                missions.append(parsed)
    return missions


def git_mission_provenance(repo_name, repo_path):
    """Extract mission→file mappings from git commit messages."""
    import subprocess
    import re

    if not (repo_path / ".git").exists():
        return []

    try:
        out = subprocess.check_output(
            ["git", "log", "--all", "--format=COMMIT:%H %s", "--name-only"],
            cwd=repo_path, text=True, stderr=subprocess.DEVNULL,
        )
    except subprocess.CalledProcessError:
        return []

    edges = []
    current_missions = []
    current_files = []

    for line in out.split("\n"):
        line = line.strip()
        if line.startswith("COMMIT:"):
            # Flush previous
            if current_missions and current_files:
                for m in current_missions:
                    for f in current_files:
                        if any(f.endswith(ext) for ext in FILE_EXTS):
                            edges.append((m, f))
            # Parse new commit
            msg = line[7:]  # after "COMMIT:"
            current_missions = re.findall(r'\bM-[a-zA-Z0-9_-]+', msg)
            current_files = []
        elif line and not line.startswith("COMMIT:"):
            current_files.append(line)

    # Flush last
    if current_missions and current_files:
        for m in current_missions:
            for f in current_files:
                if any(f.endswith(ext) for ext in FILE_EXTS):
                    edges.append((m, f))

    return edges


def file_to_namespace(filepath):
    """Infer Clojure namespace from file path, or None."""
    if not filepath.endswith((".clj", ".cljc", ".cljs")):
        return None
    # Strip src/test/dev prefix
    import re
    m = re.match(r'(?:src|test|dev)/(.*)\.(clj[cs]?)', filepath)
    if not m:
        return None
    ns_path = m.group(1)
    return ns_path.replace("/", ".").replace("_", "-")


def ingest_mission_provenance():
    """Ingest mission provenance: mission entities + mission→file edges."""
    print("\n=== ENRICHMENT L1: MISSION PROVENANCE ===")

    written = 0
    errors = 0

    # Stream A: Declarative (mission file scanning)
    print("\n  Stream A: Mission file declarations")
    missions = scan_all_missions()
    print(f"  Found {len(missions)} missions across {len(MISSION_DIRS)} repos")

    declarative_edges = []
    for m in missions:
        # Mission entity
        props = {"status": m["status"] or "unknown", "repo": m["repo"]}
        result = post_hyperedge(
            "project/mission",
            [f"mission:{m['mission_id']}"],
            props=props,
        )
        if result:
            written += 1
        else:
            errors += 1

        # Mission→file edges from :out and file refs
        for filepath in m["all_files"]:
            file_id = f"file:{m['repo']}/{filepath}"
            declarative_edges.append((m["mission_id"], file_id, filepath))

    # Deduplicate declarative edges
    seen = set()
    for mission_id, file_id, filepath in declarative_edges:
        key = (mission_id, file_id)
        if key in seen:
            continue
        seen.add(key)

        result = post_hyperedge(
            "project/mission-file",
            [f"mission:{mission_id}", file_id],
            props={"source": "declaration"},
        )
        if result:
            written += 1
        else:
            errors += 1

        # Also emit mission→namespace if it's a Clojure file
        ns = file_to_namespace(filepath)
        if ns:
            ns_key = (mission_id, ns)
            if ns_key not in seen:
                seen.add(ns_key)
                result = post_hyperedge(
                    "project/mission-namespace",
                    [f"mission:{mission_id}", f"ns:{ns}"],
                    props={"source": "declaration"},
                )
                if result:
                    written += 1
                else:
                    errors += 1

    print(f"  Declarative: {len(missions)} missions, {len(seen)} edges")

    # Stream B: Empirical (git blame)
    print("\n  Stream B: Git commit history")
    git_edge_count = 0

    for repo_name, repo_path in REPOS.items():
        edges = git_mission_provenance(repo_name, repo_path)
        for mission_id, filepath in edges:
            file_id = f"file:{repo_name}/{filepath}"
            key = (mission_id, file_id)
            if key in seen:
                continue
            seen.add(key)

            result = post_hyperedge(
                "project/mission-file",
                [f"mission:{mission_id}", file_id],
                props={"source": "git-blame"},
            )
            if result:
                written += 1
                git_edge_count += 1
            else:
                errors += 1

            ns = file_to_namespace(filepath)
            if ns:
                ns_key = (mission_id, ns)
                if ns_key not in seen:
                    seen.add(ns_key)
                    result = post_hyperedge(
                        "project/mission-namespace",
                        [f"mission:{mission_id}", f"ns:{ns}"],
                        props={"source": "git-blame"},
                    )
                    if result:
                        written += 1
                        git_edge_count += 1
                    else:
                        errors += 1

        if edges:
            missions_found = len(set(m for m, _ in edges))
            print(f"    {repo_name}: {len(edges)} raw edges, {missions_found} missions referenced")

    print(f"  Git blame: {git_edge_count} new edges (after dedup)")
    print(f"  L1 TOTAL: {written} written, {errors} errors")
    return written


def run_layer1():
    """Run enrichment Layer 1: mission provenance."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 1: Mission Provenance")
    print("=" * 60)

    before = count_hyperedges()
    total = ingest_mission_provenance()
    after = count_hyperedges()

    # Summary stats
    missions = scan_all_missions()
    missions_with_files = [m for m in missions if m["all_files"]]
    anomalies = []
    if len(missions_with_files) < len(missions) * 0.5:
        anomalies.append(f"only {len(missions_with_files)}/{len(missions)} missions have file refs")

    # Missions with most file refs
    print("\n=== MISSION PROVENANCE COVERAGE (top 10) ===")
    by_files = sorted(missions, key=lambda m: len(m["all_files"]), reverse=True)
    for m in by_files[:10]:
        n = len(m["all_files"])
        if n > 0:
            print(f"  {m['mission_id']:40s} {n:3d} files  [{m['status'] or '?'}]")

    record_enrichment_layer(1, before, after, anomalies=anomalies or None)
    print(f"\n=== LAYER 1 COMPLETE: {total} hyperedges written ===")
    return total


# --- Enrichment Layer 2: Pattern Provenance ---

LIBRARY_ROOT = Path("/home/joe/code/futon3/library")

# Pattern namespaces to scan in mission docs
PATTERN_NAMESPACES = (
    "realtime/", "social/", "gauntlet/", "agent/", "aif/",
    "stack-coherence/", "code-coherence/", "enrichment/",
    "p4ng/", "devmap-coherence/", "iching/", "musn/",
    "contributing/", "control/", "workflow-coherence/",
    "software-design/", "library-coherence/", "paramitas/",
)


def parse_flexiarg(path):
    """Parse a flexiarg file for pattern metadata and references."""
    import re

    try:
        text = path.read_text()
    except (OSError, UnicodeDecodeError):
        return None

    result = {"path": str(path)}

    # Extract @flexiarg ID
    m = re.search(r'^@flexiarg\s+(.+)', text, re.MULTILINE)
    if m:
        result["pattern_id"] = m.group(1).strip()

    # Extract @title
    m = re.search(r'^@title\s+(.+)', text, re.MULTILINE)
    if m:
        result["title"] = m.group(1).strip()

    # Extract @sigils
    m = re.search(r'^@sigils\s+\[(.+?)\]', text, re.MULTILINE)
    if m:
        result["sigils"] = m.group(1).strip()

    # Extract @references
    m = re.search(r'^@references\s+\[(.+?)\]', text, re.MULTILINE)
    if m:
        refs = [r.strip() for r in m.group(1).split() if r.strip()]
        result["references"] = refs

    # Extract @keywords
    m = re.search(r'^@keywords\s+(.+)', text, re.MULTILINE)
    if m:
        result["keywords"] = [k.strip() for k in m.group(1).split(",")]

    return result


def scan_flexiarg_library():
    """Scan the entire flexiarg library, return list of parsed patterns."""
    patterns = []
    if not LIBRARY_ROOT.exists():
        return patterns

    for flexiarg in sorted(LIBRARY_ROOT.rglob("*.flexiarg")):
        parsed = parse_flexiarg(flexiarg)
        if parsed and "pattern_id" in parsed:
            patterns.append(parsed)
    return patterns


def grep_patterns_in_missions():
    """Find pattern namespace references in mission docs."""
    import re

    pattern_rx = re.compile(
        r'\b(' + '|'.join(re.escape(ns) for ns in PATTERN_NAMESPACES) +
        r')([a-zA-Z0-9_-]+)'
    )

    edges = []  # (pattern_id, mission_id)

    for repo_name, mission_dir in MISSION_DIRS.items():
        if not mission_dir.exists():
            continue
        for md_file in sorted(mission_dir.glob("M-*.md")):
            mission_id = md_file.stem
            try:
                text = md_file.read_text()
            except (OSError, UnicodeDecodeError):
                continue

            found = set()
            for m in pattern_rx.finditer(text):
                pattern_id = m.group(1) + m.group(2)
                # Skip false positives like social/shapes.clj (those are code paths)
                if "." in m.group(2):
                    continue
                found.add(pattern_id)

            for pat in found:
                edges.append((pat, mission_id))

    return edges


def ingest_pattern_provenance():
    """Ingest pattern entities, pattern→pattern refs, and pattern→mission edges."""
    print("\n=== ENRICHMENT L2: PATTERN PROVENANCE ===")

    written = 0
    errors = 0

    # Stream A: Flexiarg library → pattern entities + pattern→pattern refs
    print("\n  Stream A: Flexiarg library scan")
    patterns = scan_flexiarg_library()
    print(f"  Found {len(patterns)} patterns in library")

    ref_count = 0
    for p in patterns:
        pid = p["pattern_id"]
        props = {}
        if p.get("title"):
            props["title"] = p["title"][:200]
        if p.get("sigils"):
            props["sigils"] = p["sigils"]
        if p.get("keywords"):
            props["keywords"] = ", ".join(p["keywords"][:10])

        # Pattern entity
        result = post_hyperedge(
            "project/pattern",
            [f"pattern:{pid}"],
            props=props or None,
        )
        if result:
            written += 1
        else:
            errors += 1

        # Pattern→pattern references
        for ref in p.get("references", []):
            result = post_hyperedge(
                "project/pattern-reference",
                [f"pattern:{pid}", f"pattern:{ref}"],
                props={"source": "flexiarg-reference"},
            )
            if result:
                written += 1
                ref_count += 1
            else:
                errors += 1

    print(f"  Patterns: {len(patterns)}, cross-references: {ref_count}")

    # Stream B: Mission docs → pattern→mission edges
    print("\n  Stream B: Pattern mentions in mission docs")
    mission_edges = grep_patterns_in_missions()
    print(f"  Found {len(mission_edges)} pattern→mission mentions")

    seen = set()
    pm_count = 0
    for pattern_id, mission_id in mission_edges:
        key = (pattern_id, mission_id)
        if key in seen:
            continue
        seen.add(key)

        result = post_hyperedge(
            "project/pattern-mission",
            [f"pattern:{pattern_id}", f"mission:{mission_id}"],
            props={"source": "text-mention"},
        )
        if result:
            written += 1
            pm_count += 1
        else:
            errors += 1

    print(f"  Unique pattern→mission edges: {pm_count}")
    print(f"  L2 TOTAL: {written} written, {errors} errors")
    return written


def run_layer2():
    """Run enrichment Layer 2: pattern provenance."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 2: Pattern Provenance")
    print("=" * 60)

    before = count_hyperedges()
    total = ingest_pattern_provenance()
    after = count_hyperedges()

    # Summary
    patterns = scan_flexiarg_library()
    mission_edges = grep_patterns_in_missions()
    unique_patterns_in_missions = len(set(p for p, _ in mission_edges))

    anomalies = []
    if unique_patterns_in_missions < 5:
        anomalies.append(f"only {unique_patterns_in_missions} unique patterns referenced in missions")

    print(f"\n=== PATTERN COVERAGE ===")
    print(f"  Library: {len(patterns)} patterns across {len(set(p['pattern_id'].split('/')[0] for p in patterns if '/' in p['pattern_id']))} namespaces")
    print(f"  Referenced in missions: {unique_patterns_in_missions} unique patterns")

    # Top referenced patterns
    from collections import Counter
    pat_counts = Counter(p for p, _ in mission_edges)
    print(f"\n  Top patterns by mission mentions:")
    for pat, count in pat_counts.most_common(10):
        print(f"    {pat:45s} {count:2d} missions")

    record_enrichment_layer(2, before, after, anomalies=anomalies or None)
    print(f"\n=== LAYER 2 COMPLETE: {total} hyperedges written ===")
    return total


# --- Layer 3: Evidence Binding ---

EVIDENCE_API = f"{FUTON3C}/api/alpha/evidence"


def fetch_all_evidence(batch_size=100):
    """Fetch all evidence entries from the futon3c evidence store."""
    entries = []
    offset = 0
    seen_ids = set()
    while True:
        url = f"{EVIDENCE_API}?limit={batch_size}&offset={offset}"
        data = get_json(url)
        batch = data.get("entries", [])
        if not batch:
            break
        new_count = 0
        for e in batch:
            eid = e.get("evidence/id")
            if eid and eid not in seen_ids:
                seen_ids.add(eid)
                entries.append(e)
                new_count += 1
        if new_count == 0:
            break  # No new entries = we've wrapped around
        offset += batch_size
    return entries


def extract_code_refs(text):
    """Extract namespace, file, and mission references from evidence text."""
    import re

    refs = {"namespaces": set(), "files": set(), "missions": set()}

    # Namespace references: futon3c.agency.core etc.
    for m in re.finditer(r'futon\d[a-z]?\.[a-z][a-z0-9._-]+', text):
        ns = m.group()
        # Skip partial matches that are just URLs or version strings
        if not re.search(r'\d+\.\d+\.\d+', ns):
            refs["namespaces"].add(ns)

    # File paths: src/futon3c/... or test/futon3c/...
    for m in re.finditer(r'(?:src|test)/futon\d[a-z]?/[a-z][a-z0-9_/.-]+\.clj[sc]?', text):
        refs["files"].add(m.group())

    # Mission references: M-some-name
    for m in re.finditer(r'M-[a-z][a-z0-9-]+', text):
        refs["missions"].add(m.group())

    return refs


def ingest_evidence_bindings():
    """Scan evidence entries for code references and create binding hyperedges."""
    print("\n--- Fetching evidence entries ---")
    entries = fetch_all_evidence()
    print(f"  Fetched {len(entries)} unique evidence entries")

    written = 0
    errors = 0
    ns_bindings = 0
    file_bindings = 0
    mission_bindings = 0
    seen = set()

    for e in entries:
        eid = e.get("evidence/id", "unknown")
        body = e.get("evidence/body", {})
        text = body.get("text", "")
        if not text:
            continue

        refs = extract_code_refs(text)
        etype = e.get("evidence/type", "unknown")
        author = e.get("evidence/author", "unknown")
        at = e.get("evidence/at", "")

        # Namespace bindings
        for ns in refs["namespaces"]:
            edge_key = f"evidence-ns:{eid}:{ns}"
            if edge_key in seen:
                continue
            seen.add(edge_key)
            result = post_hyperedge(
                "evidence/namespace-binding",
                [f"evidence:{eid}", f"namespace:{ns}"],
                props={"evidence-type": etype, "author": author, "detected-at": at},
                content={"source": "text-scan"},
                labels=["evidence", "binding", "namespace"],
            )
            if result:
                written += 1
                ns_bindings += 1
            else:
                errors += 1

        # File bindings
        for fp in refs["files"]:
            edge_key = f"evidence-file:{eid}:{fp}"
            if edge_key in seen:
                continue
            seen.add(edge_key)
            result = post_hyperedge(
                "evidence/file-binding",
                [f"evidence:{eid}", f"file:{fp}"],
                props={"evidence-type": etype, "author": author, "detected-at": at},
                content={"source": "text-scan"},
                labels=["evidence", "binding", "file"],
            )
            if result:
                written += 1
                file_bindings += 1
            else:
                errors += 1

        # Mission bindings
        for mission in refs["missions"]:
            edge_key = f"evidence-mission:{eid}:{mission}"
            if edge_key in seen:
                continue
            seen.add(edge_key)
            result = post_hyperedge(
                "evidence/mission-binding",
                [f"evidence:{eid}", f"mission:{mission}"],
                props={"evidence-type": etype, "author": author, "detected-at": at},
                content={"source": "text-scan"},
                labels=["evidence", "binding", "mission"],
            )
            if result:
                written += 1
                mission_bindings += 1
            else:
                errors += 1

    print(f"  Namespace bindings: {ns_bindings}")
    print(f"  File bindings: {file_bindings}")
    print(f"  Mission bindings: {mission_bindings}")
    print(f"  L3 TOTAL: {written} written, {errors} errors")
    return written


def run_layer3():
    """Run enrichment Layer 3: evidence binding."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 3: Evidence Binding")
    print("=" * 60)

    before = count_hyperedges()
    total = ingest_evidence_bindings()
    after = count_hyperedges()

    anomalies = []
    if total < 10:
        anomalies.append(f"only {total} evidence bindings found — evidence may not reference code explicitly")

    # Coverage summary
    entries = fetch_all_evidence()
    entries_with_refs = 0
    for e in entries:
        text = e.get("evidence/body", {}).get("text", "")
        if text:
            refs = extract_code_refs(text)
            if any(refs.values()):
                entries_with_refs += 1

    coverage_pct = (entries_with_refs / len(entries) * 100) if entries else 0
    print(f"\n=== EVIDENCE COVERAGE ===")
    print(f"  {entries_with_refs}/{len(entries)} entries reference code ({coverage_pct:.1f}%)")
    if coverage_pct < 5:
        anomalies.append(f"only {coverage_pct:.1f}% of evidence references code — most evidence is unbound chat")

    record_enrichment_layer(3, before, after, anomalies=anomalies or None)
    print(f"\n=== LAYER 3 COMPLETE: {total} hyperedges written ===")
    return total


# --- Layer 4: Tension Surface ---

def get_code_namespaces():
    """Get all namespaces from the code column in the hyperedge store."""
    import re
    body = urllib.request.urlopen(
        f"{FUTON1A}/api/alpha/hyperedges?type=code/namespace&limit=1000"
    ).read().decode()
    return set(re.findall(r'"ns:([^"]+)"', body))


def get_mission_namespaces():
    """Get all namespace→mission edges from L1."""
    import re
    body = urllib.request.urlopen(
        f"{FUTON1A}/api/alpha/hyperedges?type=project/mission-namespace&limit=1000"
    ).read().decode()
    # Return dict: namespace → set of missions
    edges = re.findall(r'"mission:([^"]+)"[^"]*"ns:([^"]+)"|"ns:([^"]+)"[^"]*"mission:([^"]+)"', body)
    ns_missions = {}
    for m1, n1, n2, m2 in edges:
        mission = m1 or m2
        ns = n1 or n2
        if mission and ns:
            ns_missions.setdefault(ns, set()).add(mission)
    return ns_missions


def get_mc_tensions():
    """Fetch live tensions from Mission Control API."""
    data = get_json(f"{FUTON3C}/api/alpha/mc/tensions")
    return data.get("tensions", [])


def ingest_tension_surface():
    """Derive cross-layer tensions and create tension hyperedges."""
    import re

    written = 0
    errors = 0
    seen = set()

    # --- Stream A: Orphan namespace tensions ---
    print("\n--- Stream A: Orphan namespace tensions ---")
    code_nss = get_code_namespaces()
    mission_ns_map = get_mission_namespaces()
    mission_claimed = set(mission_ns_map.keys())

    orphans = code_nss - mission_claimed
    print(f"  Code namespaces: {len(code_nss)}")
    print(f"  Mission-claimed: {len(mission_claimed)}")
    print(f"  Orphan namespaces: {len(orphans)}")

    orphan_count = 0
    for ns in sorted(orphans):
        edge_key = f"tension-orphan:{ns}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "tension/orphan-namespace",
            [f"ns:{ns}"],
            props={
                "tension-type": "orphan-namespace",
                "summary": f"{ns} — in code column but claimed by no mission",
            },
            content={"source": "cross-layer-L0-L1"},
            labels=["tension", "orphan", "namespace"],
        )
        if result:
            written += 1
            orphan_count += 1
        else:
            errors += 1
    print(f"  Orphan tensions created: {orphan_count}")

    # --- Stream B: Ghost claim tensions ---
    print("\n--- Stream B: Ghost claim tensions ---")
    ghosts = mission_claimed - code_nss
    print(f"  Ghost claims (mission→ns not in code): {len(ghosts)}")

    ghost_count = 0
    for ns in sorted(ghosts):
        missions = mission_ns_map.get(ns, set())
        edge_key = f"tension-ghost:{ns}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "tension/ghost-claim",
            [f"ns:{ns}"] + [f"mission:{m}" for m in sorted(missions)[:3]],
            props={
                "tension-type": "ghost-claim",
                "summary": f"{ns} — claimed by {', '.join(sorted(missions)[:3])} but not in code column",
                "mission-count": len(missions),
            },
            content={"source": "cross-layer-L0-L1"},
            labels=["tension", "ghost", "namespace"],
        )
        if result:
            written += 1
            ghost_count += 1
        else:
            errors += 1
    print(f"  Ghost claim tensions created: {ghost_count}")

    # --- Stream C: MC tension → namespace binding ---
    print("\n--- Stream C: MC tension bindings ---")
    mc_tensions = get_mc_tensions()
    print(f"  MC tensions: {len(mc_tensions)}")

    mc_bind_count = 0
    for t in mc_tensions:
        tid = f"{t.get('tension/devmap', 'unknown')}/{t.get('tension/component', 'unknown')}"
        ttype = t.get("tension/type", "unknown")
        summary = t.get("tension/summary", "")

        # Try to match component name to namespaces
        comp = t.get("tension/component", "")
        # Strip C- prefix and try matching
        comp_bare = re.sub(r'^C-', '', comp).lower().replace('-', '.')

        matching_nss = [
            ns for ns in code_nss
            if comp_bare in ns.lower() or comp.lower().replace('-', '.') in ns.lower()
        ]

        for ns in matching_nss:
            edge_key = f"tension-mc:{tid}:{ns}"
            if edge_key in seen:
                continue
            seen.add(edge_key)
            result = post_hyperedge(
                "tension/mc-code-binding",
                [f"tension:{tid}", f"ns:{ns}"],
                props={
                    "tension-type": ttype,
                    "mc-summary": summary,
                },
                content={"source": "mc-component-name-match"},
                labels=["tension", "mc", "binding"],
            )
            if result:
                written += 1
                mc_bind_count += 1
            else:
                errors += 1
    print(f"  MC→namespace bindings: {mc_bind_count}")

    # --- Stream D: Coverage gap tensions ---
    # Namespaces claimed by missions but with zero pattern provenance
    print("\n--- Stream D: Pattern coverage gaps ---")
    body = urllib.request.urlopen(
        f"{FUTON1A}/api/alpha/hyperedges?type=project/pattern-mission&limit=1000"
    ).read().decode()
    missions_with_patterns = set(re.findall(r'"mission:([^"]+)"', body))

    # All missions from L1
    body2 = urllib.request.urlopen(
        f"{FUTON1A}/api/alpha/hyperedges?type=project/mission&limit=1000"
    ).read().decode()
    all_missions = set(re.findall(r'"mission:([^"]+)"', body2))

    missions_without_patterns = all_missions - missions_with_patterns
    print(f"  Missions total: {len(all_missions)}")
    print(f"  With pattern provenance: {len(missions_with_patterns)}")
    print(f"  Without patterns: {len(missions_without_patterns)}")

    gap_count = 0
    for m in sorted(missions_without_patterns):
        edge_key = f"tension-gap:{m}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "tension/pattern-coverage-gap",
            [f"mission:{m}"],
            props={
                "tension-type": "pattern-coverage-gap",
                "summary": f"{m} — no pattern provenance in L2",
            },
            content={"source": "cross-layer-L1-L2"},
            labels=["tension", "coverage", "pattern"],
        )
        if result:
            written += 1
            gap_count += 1
        else:
            errors += 1
    print(f"  Coverage gap tensions: {gap_count}")

    print(f"\n  L4 TOTAL: {written} written, {errors} errors")
    return written


def run_layer4():
    """Run enrichment Layer 4: tension surface."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 4: Tension Surface")
    print("=" * 60)

    before = count_hyperedges()
    total = ingest_tension_surface()
    after = count_hyperedges()

    anomalies = []

    # Compute tension density
    code_nss = get_code_namespaces()
    if code_nss:
        tension_density = total / len(code_nss)
        print(f"\n=== TENSION DENSITY ===")
        print(f"  {total} tensions across {len(code_nss)} namespaces ({tension_density:.2f} per ns)")
        if tension_density > 2.0:
            anomalies.append(f"high tension density ({tension_density:.2f}/ns) — many namespaces have multiple tensions")

    record_enrichment_layer(4, before, after, anomalies=anomalies or None)
    print(f"\n=== LAYER 4 COMPLETE: {total} hyperedges written ===")
    return total


# --- Layer 5: Cross-Futon Dependencies ---

FUTON_REPOS = {
    "futon3c": Path(os.path.expanduser("~/code/futon3c")),
    "futon1a": Path(os.path.expanduser("~/code/futon1a")),
    "futon3b": Path(os.path.expanduser("~/code/futon3b")),
    "futon3a": Path(os.path.expanduser("~/code/futon3a")),
    "futon4":  Path(os.path.expanduser("~/code/futon4")),
    "futon3":  Path(os.path.expanduser("~/code/futon3")),
    "futon5":  Path(os.path.expanduser("~/code/futon5")),
    "futon2":  Path(os.path.expanduser("~/code/futon2")),
}

API_PORTS = {
    "7070": "futon3c",
    "7071": "futon1a",
    "7072": "futon3b",
    "8080": "futon4",
}


def repo_of_ns(ns):
    """Determine which repo a namespace belongs to based on prefix."""
    parts = ns.split(".")
    prefix = parts[0]
    if prefix.startswith("futon"):
        return prefix
    return "other"


def scan_deps_edn():
    """Parse deps.edn files across repos for classpath-level dependencies."""
    import re
    deps = []
    for repo, path in FUTON_REPOS.items():
        deps_file = path / "deps.edn"
        if not deps_file.exists():
            continue
        content = deps_file.read_text()
        for m in re.finditer(r'(\w+)/\w+\s+\{:local/root\s+"\.\./([\w-]+)"', content):
            dep_name = m.group(2)
            deps.append((repo, dep_name, "classpath"))
    return deps


def scan_cross_repo_requires():
    """Get cross-repo namespace require edges from the hyperedge store."""
    import re
    body = urllib.request.urlopen(
        f"{FUTON1A}/api/alpha/hyperedges?type=code/requires&limit=1000"
    ).read().decode()

    edges = []
    for m in re.finditer(r':hx/endpoints\s+\["ns:([^"]+)"\s+"ns:([^"]+)"\]', body):
        a, b = m.group(1), m.group(2)
        ra, rb = repo_of_ns(a), repo_of_ns(b)
        if ra != rb:
            edges.append((a, b, ra, rb))
    return edges


def scan_http_api_deps():
    """Scan source and scripts for HTTP API calls to other futon services."""
    deps = []

    for repo, base_path in FUTON_REPOS.items():
        for search_dir in [base_path / "src", base_path / "scripts"]:
            if not search_dir.exists():
                continue
            for root, dirs, files in os.walk(search_dir):
                for f in files:
                    if not any(f.endswith(ext) for ext in
                               ('.clj', '.cljs', '.cljc', '.py', '.el', '.sh', '.bb')):
                        continue
                    fpath = Path(root) / f
                    try:
                        content = fpath.read_text()
                    except Exception:
                        continue
                    for port, target_repo in API_PORTS.items():
                        if target_repo == repo:
                            continue
                        if f":{port}" in content or f"localhost:{port}" in content:
                            rel = str(fpath.relative_to(base_path))
                            deps.append((repo, target_repo, rel, "http-api"))
    return deps


def ingest_cross_futon_deps():
    """Create cross-futon dependency hyperedges from three streams."""
    written = 0
    errors = 0
    seen = set()

    # --- Stream A: Classpath dependencies ---
    print("\n--- Stream A: Classpath dependencies (deps.edn) ---")
    classpath_deps = scan_deps_edn()
    print(f"  Found {len(classpath_deps)} classpath deps")

    cp_count = 0
    for src_repo, dep_repo, _ in classpath_deps:
        edge_key = f"dep-classpath:{src_repo}:{dep_repo}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "dep/classpath",
            [f"repo:{src_repo}", f"repo:{dep_repo}"],
            props={
                "dep-type": "classpath",
                "summary": f"{src_repo} depends on {dep_repo} via deps.edn :local/root",
            },
            content={"source": "deps-edn-scan"},
            labels=["dependency", "classpath", "cross-futon"],
        )
        if result:
            written += 1
            cp_count += 1
        else:
            errors += 1
    print(f"  Classpath dep edges: {cp_count}")

    # --- Stream B: Cross-repo namespace requires ---
    print("\n--- Stream B: Cross-repo namespace requires ---")
    ns_deps = scan_cross_repo_requires()
    print(f"  Found {len(ns_deps)} cross-repo require edges")

    ns_count = 0
    for ns_from, ns_to, repo_from, repo_to in ns_deps:
        edge_key = f"dep-ns:{ns_from}:{ns_to}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "dep/cross-repo-require",
            [f"ns:{ns_from}", f"ns:{ns_to}", f"repo:{repo_from}", f"repo:{repo_to}"],
            props={
                "dep-type": "namespace-require",
                "from-repo": repo_from,
                "to-repo": repo_to,
                "summary": f"{ns_from} ({repo_from}) requires {ns_to} ({repo_to})",
            },
            content={"source": "hyperedge-store-cross-ref"},
            labels=["dependency", "require", "cross-futon"],
        )
        if result:
            written += 1
            ns_count += 1
        else:
            errors += 1
    print(f"  Cross-repo require edges: {ns_count}")

    # --- Stream C: HTTP API dependencies ---
    print("\n--- Stream C: HTTP API dependencies ---")
    api_deps = scan_http_api_deps()
    print(f"  Found {len(api_deps)} HTTP API deps")

    api_count = 0
    for src_repo, tgt_repo, filepath, _ in api_deps:
        edge_key = f"dep-api:{src_repo}:{tgt_repo}:{filepath}"
        if edge_key in seen:
            continue
        seen.add(edge_key)
        result = post_hyperedge(
            "dep/http-api",
            [f"repo:{src_repo}", f"repo:{tgt_repo}", f"file:{src_repo}/{filepath}"],
            props={
                "dep-type": "http-api",
                "from-repo": src_repo,
                "to-repo": tgt_repo,
                "summary": f"{src_repo}/{filepath} calls {tgt_repo} API",
            },
            content={"source": "port-scan"},
            labels=["dependency", "http-api", "cross-futon"],
        )
        if result:
            written += 1
            api_count += 1
        else:
            errors += 1
    print(f"  HTTP API dep edges: {api_count}")

    print(f"\n  L5 TOTAL: {written} written, {errors} errors")
    return written


def run_layer5():
    """Run enrichment Layer 5: cross-futon dependencies."""
    print("\n" + "=" * 60)
    print("  ENRICHMENT LAYER 5: Cross-Futon Dependencies")
    print("=" * 60)

    before = count_hyperedges()
    total = ingest_cross_futon_deps()
    after = count_hyperedges()

    anomalies = []

    # Dependency graph summary
    classpath = scan_deps_edn()
    ns_deps = scan_cross_repo_requires()
    api_deps = scan_http_api_deps()

    repos_mentioned = set()
    for src, tgt, _ in classpath:
        repos_mentioned.update([src, tgt])
    for _, _, src, tgt in ns_deps:
        repos_mentioned.update([src, tgt])
    for src, tgt, _, _ in api_deps:
        repos_mentioned.update([src, tgt])

    print(f"\n=== CROSS-FUTON CONNECTIVITY ===")
    print(f"  Repos in dependency graph: {len(repos_mentioned)}")
    print(f"  Classpath deps: {len(classpath)}")
    print(f"  Cross-repo requires: {len(ns_deps)}")
    print(f"  HTTP API deps: {len(api_deps)}")

    # Check for I-5 violations (futon3c -> futon3 deps)
    i5_violations = [(a, b, ra, rb) for a, b, ra, rb in ns_deps
                     if ra == "futon3c" and rb == "futon3"]
    if i5_violations:
        anomalies.append(
            f"I-5 VIOLATION: {len(i5_violations)} futon3c->futon3 namespace requires"
        )
        print(f"\n  WARNING: I-5 violations detected:")
        for a, b, _, _ in i5_violations:
            print(f"    {a} -> {b}")

    record_enrichment_layer(5, before, after, anomalies=anomalies or None)
    print(f"\n=== LAYER 5 COMPLETE: {total} hyperedges written ===")
    return total


# --- Count ---

def count_hyperedges():
    """Report current hyperedge counts by type."""
    print("\n=== HYPEREDGE COUNTS ===")

    types = [
        # Math
        "math/post", "math/scope", "math/expression",
        "math/iatc", "math/mention", "math/discourse",
        "math/surface", "math/categorical",
        # Code
        "code/namespace", "code/var", "code/ns-contains-var", "code/requires",
        # Project
        "project/devmap", "project/component", "project/devmap-contains",
        "project/tension", "project/tension-on", "project/trace-path",
        # Invariant violations
        "invariant/undocumented-entry-point",
        "invariant/uncovered-component",
        "invariant/orphan-namespace",
        "invariant/ungrounded-definition",
        # Enrichment L0
        "code/file-churn",
        "code/indentation-complexity",
        # Enrichment L1
        "project/mission",
        "project/mission-file",
        "project/mission-namespace",
        # Enrichment L2
        "project/pattern",
        "project/pattern-reference",
        "project/pattern-mission",
        # Enrichment L3
        "evidence/namespace-binding",
        "evidence/file-binding",
        "evidence/mission-binding",
        # Enrichment L4
        "tension/orphan-namespace",
        "tension/ghost-claim",
        "tension/mc-code-binding",
        "tension/pattern-coverage-gap",
        # Enrichment L5
        "dep/classpath",
        "dep/cross-repo-require",
        "dep/http-api",
        # Meta
        "meta/enrichment-layer",
        # Test
        "test/ping",
    ]

    total = 0
    for t in types:
        count = query_hyperedge_count(t)
        if count > 0:
            print(f"  {t}: {count}")
            total += count

    print(f"\n  TOTAL: {total}")
    return total


# --- Main ---

def main():
    parser = argparse.ArgumentParser(description="Ingest three-column data into futon1a")
    parser.add_argument("--math", action="store_true", help="Ingest math column")
    parser.add_argument("--code", action="store_true", help="Ingest code column")
    parser.add_argument("--project", action="store_true", help="Ingest project column")
    parser.add_argument("--all", action="store_true", help="Ingest all columns")
    parser.add_argument("--invariants", action="store_true", help="Run cross-column invariant checks")
    parser.add_argument("--layer0", action="store_true", help="Run enrichment Layer 0")
    parser.add_argument("--layer1", action="store_true", help="Run enrichment Layer 1 (mission provenance)")
    parser.add_argument("--layer2", action="store_true", help="Run enrichment Layer 2 (pattern provenance)")
    parser.add_argument("--layer3", action="store_true", help="Run enrichment Layer 3 (evidence binding)")
    parser.add_argument("--layer4", action="store_true", help="Run enrichment Layer 4 (tension surface)")
    parser.add_argument("--layer5", action="store_true", help="Run enrichment Layer 5 (cross-futon deps)")
    parser.add_argument("--count", action="store_true", help="Report current counts")
    args = parser.parse_args()

    if not any([args.math, args.code, args.project, args.all, args.count, args.invariants, args.layer0, args.layer1, args.layer2, args.layer3, args.layer4, args.layer5]):
        parser.print_help()
        sys.exit(1)

    total = 0

    if args.count:
        count_hyperedges()
        return

    if args.math or args.all:
        total += ingest_math_column()

    if args.code or args.all:
        total += ingest_code_column()

    if args.project or args.all:
        total += ingest_project_column()

    if args.invariants or args.all:
        check_invariants()

    if args.layer0:
        total += run_layer0()

    if args.layer1:
        total += run_layer1()

    if args.layer2:
        total += run_layer2()

    if args.layer3:
        total += run_layer3()

    if args.layer4:
        total += run_layer4()

    if args.layer5:
        total += run_layer5()

    if total > 0:
        print(f"\n=== GRAND TOTAL: {total} hyperedges written ===")

    # Verify round-trip
    print("\nVerifying round-trip...")
    count_hyperedges()


if __name__ == "__main__":
    main()
