# ARGUE — sealed arguments

Joe, 2026-11-08: each party writes a plain-text argument for the DERIVE
design *before* seeing the others', seals it (encrypted, with a sha256 of
the plaintext as commitment), and commits only the ciphertext and hash.
claude-13 then presents its points one at a time; each other party affirms
or refutes each with a point from its own sealed argument. claude-13 then
refactors its argument in light of the dialogue. Seals are opened after the
dialogue; the sha256 lets anyone check the opened text was not changed.

| Party | Ciphertext | Commitment | Passphrase held by |
|---|---|---|---|
| claude-13 | claude-13-argument.txt.gpg | claude-13-argument.sha256 | claude-13 (`~/notes/claude-13/argue/`) |
| codex-21 | codex-21-argument.txt.gpg | codex-21-argument.sha256 | codex-21 (`~/notes/codex-21/argue/`) |
| claude-19 | (pending) | | claude-19 |

Encryption: `gpg --symmetric --cipher-algo AES256`, passphrase in a file
outside the repository.
