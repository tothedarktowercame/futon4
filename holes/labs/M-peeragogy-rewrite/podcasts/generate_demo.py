#!/usr/bin/env python3
"""generate_peeragogy_demo.py — build the Peeragogy podcast transcript demo page.

Reads manifest.json + transcript files, extracts a short excerpt from each,
and generates a single self-contained HTML index page.
"""
import json
import html
from pathlib import Path

ROOT = Path("/home/joe/code/futon4/holes/labs/M-peeragogy-rewrite/podcasts")
MANIFEST = ROOT / "manifest.json"
OUT = ROOT / "peeragogy-transcripts-demo.html"
OUT_DIR = ROOT / "demo-site"

def extract_excerpt(transcript_path, max_chars=300):
    """Pull the first meaningful text block from a transcript."""
    text = transcript_path.read_text(errors="ignore").strip()
    # Take first meaningful paragraph-ish chunk
    lines = [l.strip() for l in text.split("\n") if l.strip()]
    chunk = " ".join(lines[:8])  # first ~8 lines
    if len(chunk) > max_chars:
        chunk = chunk[:max_chars].rsplit(" ", 1)[0] + "…"
    return chunk

def fmt_duration(seconds):
    m, s = divmod(seconds, 60)
    h = m // 60
    m = m % 60
    if h > 0:
        return f"{h}h {m}m"
    return f"{m}m"

def generate_transcript_page(ep, transcript_text):
    """Generate a full transcript HTML page for a single episode."""
    title = html.escape(ep["title"])
    duration = fmt_duration(ep["duration_seconds"])
    url = html.escape(ep["url"])
    # Split transcript into paragraphs for readability
    lines = [html.escape(l.strip()) for l in transcript_text.split("\n") if l.strip()]
    paragraphs = []
    for i in range(0, len(lines), 5):
        para = " ".join(lines[i:i+5])
        paragraphs.append(f"<p>{para}</p>")
    body = "\n".join(paragraphs)

    return f'''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title} — Full Transcript</title>
<style>
  :root {{ --bg: #f8f6f1; --card: #fff; --accent: #c65535; --text: #2a2a2a; --muted: #888; }}
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{ font-family: Georgia, 'Times New Roman', serif; background: var(--bg); color: var(--text); line-height: 1.8; }}
  header {{ background: var(--accent); color: #fff; padding: 1.5rem; text-align: center; }}
  header h1 {{ font-size: 1.4rem; margin-bottom: 0.3rem; }}
  header a {{ color: #fff; }}
  .meta {{ text-align: center; padding: 1rem; color: var(--muted); font-size: 0.9rem; }}
  .meta a {{ color: var(--accent); text-decoration: none; }}
  main {{ max-width: 750px; margin: 0 auto; padding: 1rem 1.5rem 3rem; }}
  main p {{ margin-bottom: 1rem; font-size: 0.95rem; }}
  .back {{ display: inline-block; margin: 1rem 0; font-family: sans-serif; font-size: 0.85rem; color: var(--accent); text-decoration: none; }}
  .back:hover {{ text-decoration: underline; }}
</style>
</head>
<body>
<header>
  <h1>{title}</h1>
  <p>Episode {ep["index"]} · {duration}</p>
</header>
<div class="meta">
  <a href="{url}" target="_blank" rel="noopener">▶ Watch on YouTube</a>
</div>
<main>
  <a href="index.html" class="back">← Back to all episodes</a>
  {body}
  <a href="index.html" class="back">← Back to all episodes</a>
</main>
</body>
</html>'''

def main():
    import shutil
    # Clean and create output dir
    if OUT_DIR.exists():
        shutil.rmtree(OUT_DIR)
    OUT_DIR.mkdir(parents=True)

    manifest = json.loads(MANIFEST.read_text())
    episodes = sorted(manifest["episodes"].values(), key=lambda e: e["index"])

    cards = []
    for ep in episodes:
        tid = ep.get("transcript_path", "")
        tpath = ROOT / tid if tid else None
        excerpt = ""
        full_text = ""
        if tpath and tpath.exists():
            full_text = tpath.read_text(errors="ignore")
            excerpt = extract_excerpt(tpath)

        duration = fmt_duration(ep["duration_seconds"])
        title = html.escape(ep["title"])
        url = html.escape(ep["url"])
        yt_id = ep["url"].split("v=")[-1] if "v=" in ep["url"] else ""
        excerpt_esc = html.escape(excerpt)
        transcript_href = f"ep-{ep['index']}.html"

        # Generate per-episode transcript page
        if full_text:
            ep_page = generate_transcript_page(ep, full_text)
            (OUT_DIR / f"ep-{ep['index']}.html").write_text(ep_page)

        card = f'''
    <article class="episode-card" id="ep-{ep["index"]}">
      <div class="card-header">
        <span class="ep-num">Ep {ep["index"]}</span>
        <span class="duration">{duration}</span>
      </div>
      <h3>{title}</h3>
      <div class="excerpt">{excerpt_esc}</div>
      <div class="links">
        <a href="{transcript_href}">📄 Full transcript</a>
        · <a href="{url}" target="_blank" rel="noopener">▶ YouTube</a>
        <span class="yt-id">[{yt_id}]</span>
      </div>
    </article>'''
        cards.append(card)

    total_duration = sum(e["duration_seconds"] for e in episodes)

    page = f'''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Peeragogy Podcast — Transcript Index</title>
<style>
  :root {{ --bg: #f8f6f1; --card: #fff; --accent: #c65535; --text: #2a2a2a; --muted: #888; }}
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{ font-family: Georgia, 'Times New Roman', serif; background: var(--bg); color: var(--text); line-height: 1.6; }}
  header {{ background: var(--accent); color: #fff; padding: 2rem; text-align: center; }}
  header h1 {{ font-size: 1.8rem; margin-bottom: 0.3rem; }}
  header p {{ opacity: 0.9; font-size: 0.95rem; }}
  .summary {{ max-width: 800px; margin: 1.5rem auto; text-align: center; color: var(--muted); font-size: 0.9rem; }}
  main {{ max-width: 900px; margin: 0 auto; padding: 0 1rem 3rem; }}
  .episode-card {{
    background: var(--card); border-radius: 8px; padding: 1.2rem 1.5rem;
    margin-bottom: 1.2rem; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    border-left: 4px solid var(--accent);
  }}
  .card-header {{ display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem; }}
  .ep-num {{ background: var(--accent); color: #fff; padding: 2px 10px; border-radius: 12px; font-size: 0.8rem; font-family: sans-serif; }}
  .duration {{ color: var(--muted); font-size: 0.85rem; font-family: sans-serif; }}
  .episode-card h3 {{ font-size: 1.15rem; margin-bottom: 0.6rem; color: var(--text); }}
  .excerpt {{ font-size: 0.9rem; color: #555; margin-bottom: 0.8rem; font-style: italic; }}
  .links {{ font-size: 0.85rem; font-family: sans-serif; }}
  .links a {{ color: var(--accent); text-decoration: none; }}
  .links a:hover {{ text-decoration: underline; }}
  .yt-id {{ color: var(--muted); margin-left: 0.5rem; }}
  footer {{ text-align: center; padding: 2rem; color: var(--muted); font-size: 0.8rem; }}
</style>
</head>
<body>
<header>
  <h1>Peeragogy Podcast — Transcript Index</h1>
  <p>Peer learning + peer production, in conversation</p>
</header>
<div class="summary">
  {len(episodes)} episodes · {fmt_duration(total_duration)} total ·
  <a href="https://www.youtube.com/playlist?list=PLJ8LSNXjMu5q5qfU3nwnv8uqK1Z5Z5Z5Z">YouTube playlist</a>
</div>
<main>
{"".join(cards)}
</main>
<footer>
  Generated from manifest.json + transcripts · Peeragogy Project
</footer>
</body>
</html>'''

    OUT.write_text(page)
    index_out = OUT_DIR / "index.html"
    index_out.write_text(page)
    print(f"Wrote {OUT}")
    print(f"Wrote {index_out} (+ {len(episodes)} episode pages)")
    print(f"{len(episodes)} episodes, {fmt_duration(total_duration)} total")

if __name__ == "__main__":
    main()
