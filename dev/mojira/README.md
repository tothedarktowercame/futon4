
### Demo B: benefit filings

After merging, run `npm ci` in `dev/mojira` (ramo is pinned to 0.1.2), then
**Developer: Reload Window**. This packet does not reload the running extension.
Open **Mojira: Open**, then the home link **Demo B: benefit filings**, or paste
`mojira://bf` into Mojira's `:` prompt. A detail identifier is
`mojira://bf/<URI-encoded-benefit-record-id>`; the existing `y` copy command
produces this form. No new global shortcode is registered.

`mojira.catalogueUrl` optionally names the loopback `/arxana/v1` endpoint.
By default the extension host reads the port from
`/run/user/<uid>/futon4a-catalogue-<uid>/ready.edn`. Start futon4a's catalogue
launcher first. Only `:cascade/benefit-filings` is sent, with the current valid
time; neither the webview nor this client writes records. The webview has no
network access. Requests have a 180-second timeout and refusals are displayed.

Open a benefit with RET or f hints to read it. Merely selecting it for the
right-hand preview does not request its detail. A reading indicator remains
while the whole page is acquired. `r` reads again; `c` cycles raw records.
`j/k`, f hints, RET and Tab work on evidence links: their returned records and
attestations open in the right column. The substrate identities are a separate
lane, never joined to the item names. Unnamed serving items remain visible.

The host's finite-ground ramo relation mirrors
`futon3c.arxana.fibre-check/benefit-fibres`: each reading has a record id, an
evidence vector (possibly empty), and complete evidence acquisition. This is
not a claim that every attestation is verified or that the evidence is true.
Incomplete responses do not pass the check. There is no search bound or timeout
on the finite check; all reading positions are enumerated.

The backend currently returns the whole detail, including evidence. Opening an
evidence row reveals those already-returned facts; a per-row backend read is
still needed to avoid acquiring a whole slow page. The Emacs tracker's static
“not built” declaration lives in futon3c and needs updating after review; the
existing `mojira://bf` convention supplies its navigation target.
