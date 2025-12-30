# Engineering Contracts

Fine-grained invariants and constraints that apply across the Futon Stack.
These are smaller than devmap prototypes but must be enforced consistently.

## Format

Each contract has:
- **ID**: Short identifier (e.g., `EC-001`)
- **Statement**: The invariant in imperative form
- **Rationale**: Why this matters
- **Verification**: How to check compliance
- **References**: Related scripts, docs, or tests

---

## EC-001: Non-interactive Install

**Statement**: Installation scripts must be one-shot and fully non-interactive.

**Rationale**: Enables reproducible CI/CD, VM provisioning, and onboarding without
manual intervention. Interactive prompts (e.g., dpkg configuration dialogs) break
automation and make setup non-deterministic.

**Verification**:
```bash
# Must complete without stdin:
echo "" | ./scripts/setup-dev-env.sh
# Exit code 0, no hangs
```

**References**:
- `scripts/setup-dev-env.sh` — uses `DEBIAN_FRONTEND=noninteractive`
- `docs/dev-environment-setup.md`

---

## EC-002: (Template)

**Statement**: [Invariant here]

**Rationale**: [Why]

**Verification**: [How to check]

**References**: [Files/tests]

---

*Add new contracts above the template.*
