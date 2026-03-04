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
    parser.add_argument("--count", action="store_true", help="Report current counts")
    args = parser.parse_args()

    if not any([args.math, args.code, args.project, args.all, args.count, args.invariants]):
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

    if total > 0:
        print(f"\n=== GRAND TOTAL: {total} hyperedges written ===")

    # Verify round-trip
    print("\nVerifying round-trip...")
    count_hyperedges()


if __name__ == "__main__":
    main()
