#!/usr/bin/env python3
"""Recover typed provenance for every frozen N1 empty-reason rewrite row."""
import argparse, csv, gzip, glob, json, re, types
from collections import Counter, defaultdict
from pathlib import Path

from mfuton.lean.blueprint_render.blueprint_printer import make_blueprint_printer
from mfuton.lean.blueprint_render.library_statements import LibraryStatements
from mfuton.lean.blueprint_render.paper_proof_walk.render import proof_steps
from mfuton.lean.blueprint_render.paper_walk import RewriteReason, RewriteTransition, apply as apply_policy
from mfuton.lean.blueprint_render.paper_proof_walk import helpers
from mfuton.lean.latex_pp import _expr
from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
from mfuton.lean.entity_surface.source_extraction.population_pipeline import EmitPythonEntitySurfaceProvider
from mfuton.lean.latex_pp.latex_data import strip_entity_links

ROOT = Path('/home/joe/worktrees/mfuton-share/codex-5-N1')
CORPUS = Path('/home/joe/worktrees/mfuton-share/codex4-r4/data/tmp/apm-proofs-typed')
LIBRARY = Path('/home/joe/worktrees/mfuton-share/codex4-r4/data/tmp/apm-library-decls-robformat-2')
SOURCE = Path('/home/joe/worktrees/apm-lean/codex25-all-proofs-build')
FREEZE = '/home/joe/storage/apm-endgame/freeze-a6e043d3/render-0?.jsonl.gz'
OUT = Path('/tmp/c10-n1')
EMPTY = re.compile(r'(?m)^([^\n]*&& \\text\{\}[^\n]*)$')

# These proof heads merely expose a definition/identity, or certify arithmetic
# already visible in the adjacent expressions.  The list is typed-head based.
DIRECT = {
    '', 'id', 'rfl', 'Eq.refl', 'Iff.rfl', 'propext',
    'Eq.trans', 'Eq.symm', 'Eq.mp', 'Eq.mpr', 'cast',
}
DIRECT_PREFIX = (
    'Mathlib.Tactic.', 'tactic:',
)
ARITHMETIC = re.compile(
    r'(^|\.)(ring|ring_nf|norm_num|omega|linarith|nlinarith|positivity|field_simp)$')

def frozen_targets():
    records = {}
    for path in sorted(glob.glob(FREEZE)):
        with gzip.open(path, 'rt') as f:
            for line in f:
                row = json.loads(line); proof = row.get('proof') or ''
                hits = EMPTY.findall(proof)
                if hits:
                    records[(row['module'], row['declaration'])] = hits
    return records

def module_source(module):
    return (SOURCE / Path(*module.split('.'))).with_suffix('.lean')

def proof_head(reason, tactic, capture):
    if isinstance(reason, RewriteReason) and reason.declaration:
        return reason.declaration
    if isinstance(reason, str) and reason:
        return reason
    heads = [x['head'] or x['reason'] for x in capture if x['head'] or x['reason']]
    if heads and len(set(heads)) == 1:
        return heads[0]
    return ('tactic:' + tactic) if tactic else ''

def classify(head, transition, matched_raw):
    if not matched_raw:
        return '', '', 'no unique typed raw transition for the rendered row'
    if head in DIRECT or any(head.startswith(p) for p in DIRECT_PREFIX) or ARITHMETIC.search(head):
        return 'd', '', ''
    if head:
        return 'f', head, ''
    if transition.before_expr is not None or transition.after_expr is not None:
        return 'd', '', ''
    return 'o', '', 'untyped normalization/chain-order bridge'

def main():
    ap = argparse.ArgumentParser(); ap.add_argument('--part', type=int, default=0); ap.add_argument('--parts', type=int, default=1)
    args = ap.parse_args()
    targets = frozen_targets()
    selected = [k for i,k in enumerate(sorted(targets)) if i % args.parts == args.part]
    path=OUT/f'part-{args.part:02d}.tsv'
    fields=['module','declaration','row text','typed proof head','class','fact used','o phrase','unclassified reason']
    done = set()
    if path.exists():
        with path.open() as f:
            done = {(r['module'], r['declaration']) for r in csv.DictReader(f, delimiter='\t')}
    else:
        with path.open('w') as f:
            csv.DictWriter(f,fields,delimiter='\t',lineterminator='\n').writeheader()
    provider = EmitPythonEntitySurfaceProvider('', corpus_root=str(CORPUS))
    library = LibraryStatements(LIBRARY)
    cache = {}
    output = []
    for ordinal, (module, declaration) in enumerate(selected, 1):
        if (module, declaration) in done:
            continue
        declaration_output = []
        if module not in cache:
            constants = provider.elaborated_constants_for_module(module)
            table = {ci.name.dotted(): ci for ci in constants}
            layer = with_module_namespaces(project_display_layer('apm-proofs', home=ROOT), constants)
            printer = make_blueprint_printer(layer, omit_binders=False, const_table=table, dep_table=library)
            cache[module] = table, printer
        table, printer = cache[module]
        captures = []
        original = printer.proof_rewrite_transition
        def capture_transition(self, before, after, equality):
            transition = original(before, after, equality)
            reason, _premises, species = helpers._read_justification(equality)
            head, _args = helpers._spine(helpers._unwrap_plumbing(equality))
            captures.append({'before': strip_entity_links(transition.before),
                             'after': strip_entity_links(transition.after),
                             'reason': reason, 'species': species,
                             'head': _expr.const_name(head) if _expr.is_const(head) else _expr.ctor_of(head)})
            return transition
        printer.proof_rewrite_transition = types.MethodType(capture_transition, printer)
        try:
            raw_steps = proof_steps(table[declaration], printer, module_source(module))
        except Exception as exc:
            for text in targets[(module,declaration)]:
                declaration_output.append({'module':module,'declaration':declaration,'row text':text,
                               'typed proof head':'','class':'','fact used':'','o phrase':'',
                               'unclassified reason':f'proof_steps failed: {type(exc).__name__}: {exc}'})
            with path.open('a') as f:
                csv.DictWriter(f,fields,delimiter='\t',lineterminator='\n').writerows(declaration_output)
            output.extend(declaration_output); continue
        finally:
            printer.proof_rewrite_transition = original
        raw = []
        for step in raw_steps:
            for tr in step.rewrite_chain:
                if isinstance(tr, RewriteTransition): raw.append(tr)
        annotated = apply_policy(raw_steps, printer, module_source(module), declaration.rsplit('.',1)[-1], {})
        final = [tr for step in annotated for tr in step.rewrite_chain
                 if isinstance(tr, RewriteTransition) and not tr.reason]
        frozen = targets[(module,declaration)]
        # The final empty transitions and rendered empty rows are emitted in order.
        if len(final) != len(frozen):
            for text in frozen:
                declaration_output.append({'module':module,'declaration':declaration,'row text':text,
                               'typed proof head':'','class':'','fact used':'','o phrase':'',
                               'unclassified reason':f'count mismatch: {len(final)} typed empty transitions vs {len(frozen)} frozen rows'})
            with path.open('a') as f:
                csv.DictWriter(f,fields,delimiter='\t',lineterminator='\n').writerows(declaration_output)
            output.extend(declaration_output); continue
        used = Counter()
        for text, tr in zip(frozen, final):
            b,a = strip_entity_links(tr.before), strip_entity_links(tr.after)
            matches = [x for x in raw if ((strip_entity_links(x.before),strip_entity_links(x.after)) in ((b,a),(a,b)))]
            key=(b,a); idx=used[key]; used[key]+=1
            matched = matches[idx] if idx < len(matches) else None
            cap = [x for x in captures if (x['before'],x['after']) in ((b,a),(a,b))]
            head = proof_head(matched.reason if matched else tr.reason,
                              matched.tactic if matched else tr.tactic, cap)
            cls,fact,unc = classify(head, matched or tr, matched is not None)
            declaration_output.append({'module':module,'declaration':declaration,'row text':text,
                           'typed proof head':head,'class':cls,'fact used':fact,
                           'o phrase':'untyped normalization/chain-order bridge' if cls=='o' else '',
                           'unclassified reason':unc})
        with path.open('a') as f:
            csv.DictWriter(f,fields,delimiter='\t',lineterminator='\n').writerows(declaration_output)
        output.extend(declaration_output)
        if ordinal % 25 == 0: print(args.part, ordinal, '/', len(selected), flush=True)
    print(path, len(output), Counter(x['class'] for x in output))

if __name__ == '__main__': main()
