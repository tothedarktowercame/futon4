#!/usr/bin/env python3
"""
peeragogy-podcasts.py

Pull and transcribe the Peeragogy podcast playlist as MAP-stage source
material for M-peeragogy-rewrite. Idempotent: skips already-downloaded
audio and already-transcribed transcripts.

Pipeline per episode:
  1. yt-dlp → bestaudio → m4a (or webm/opus fallback) under audio/
  2. faster-whisper (via futon0/scripts/zoom_transcribe.py) → text under transcripts/
  3. update manifest.json with id, title, url, duration, paths, status

Outputs land under:
  futon4/holes/labs/M-peeragogy-rewrite/podcasts/
    audio/<id>.m4a
    transcripts/<id>/<id>_full.txt   (+ chunk files in chunks/, chunks_txt/)
    manifest.json

Usage:
  bin/peeragogy-podcasts.py --list                       # show playlist, no fetch
  bin/peeragogy-podcasts.py --only <id>                  # one episode end-to-end
  bin/peeragogy-podcasts.py                              # all episodes (resumable)
  bin/peeragogy-podcasts.py --transcribe-only            # skip download, only transcribe cached audio
  bin/peeragogy-podcasts.py --device cuda                # if CUDA is available

Mission: M-peeragogy-rewrite (MAP, semi-in/semi-out source).
"""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
from pathlib import Path

PLAYLIST_URL = (
    "https://www.youtube.com/playlist?list=PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE"
)

REPO_ROOT = Path(os.environ.get("FUTON4_ROOT", "/home/joe/code/futon4"))
LAB_ROOT = REPO_ROOT / "holes" / "labs" / "M-peeragogy-rewrite" / "podcasts"
AUDIO_DIR = LAB_ROOT / "audio"
TRANSCRIPT_DIR = LAB_ROOT / "transcripts"
MANIFEST_PATH = LAB_ROOT / "manifest.json"

ZOOM_TRANSCRIBE = Path("/home/joe/code/futon0/scripts/zoom_transcribe.py")
YT_DLP = "/home/joe/opt/yt-dlp/yt-dlp"
# faster-whisper lives in the voice-typing venv, not on system Python.
WHISPER_PYTHON = "/home/joe/opt/voice-typing-linux/venv/bin/python"

# initial_prompt seeded with the recurring cast and core peeragogy
# vocabulary; this dramatically improves proper-noun recognition for
# faster-whisper-small without retraining.
PEERAGOGY_PROMPT = (
    "Peeragogy podcast with Joe Corneli, Charlotte Pierce, "
    "Charles Jeffrey Danoff, Howard Rheingold, Paola Ricaurte, "
    "Lisa McDonald, Roland Legrand, Gigi Johnson, David Preston, "
    "Fabrizio Terzi, Bryan Alexander. Vocabulary: peeragogy, "
    "paragogy, polycentric, Wikiversity, P2PU, Heartbeat pattern, "
    "Wrapper pattern, Newcomer pattern, peer learning, "
    "co-learning, peeragogue."
)


def list_playlist() -> list[dict]:
    """Return [{'id','title','duration','index'}, ...] via yt-dlp --flat-playlist."""
    cmd = [
        YT_DLP,
        "--flat-playlist",
        "--print",
        "%(playlist_index)s|%(id)s|%(duration)s|%(title)s",
        PLAYLIST_URL,
    ]
    out = subprocess.run(cmd, check=True, capture_output=True, text=True).stdout
    episodes = []
    for line in out.strip().splitlines():
        if not line or "|" not in line:
            continue
        idx, vid, dur, title = line.split("|", 3)
        try:
            dur_int = int(dur)
        except ValueError:
            dur_int = 0
        episodes.append(
            {
                "index": int(idx),
                "id": vid,
                "duration": dur_int,
                "title": title.strip(),
                "url": f"https://www.youtube.com/watch?v={vid}",
            }
        )
    episodes.sort(key=lambda e: e["index"])
    return episodes


def load_manifest() -> dict:
    if MANIFEST_PATH.exists():
        return json.loads(MANIFEST_PATH.read_text())
    return {"episodes": {}}


def save_manifest(manifest: dict) -> None:
    LAB_ROOT.mkdir(parents=True, exist_ok=True)
    MANIFEST_PATH.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n")


def audio_path_for(vid: str) -> Path:
    # yt-dlp may pick m4a / webm / opus depending on availability; we accept any.
    candidates = sorted(AUDIO_DIR.glob(f"{vid}.*"))
    for p in candidates:
        if p.suffix.lower() in {".m4a", ".mp4", ".webm", ".opus", ".mp3", ".ogg"}:
            return p
    return AUDIO_DIR / f"{vid}.m4a"  # default expected path before download


def download_audio(ep: dict) -> Path:
    """Download bestaudio for episode; return path. Idempotent."""
    AUDIO_DIR.mkdir(parents=True, exist_ok=True)
    existing = sorted(AUDIO_DIR.glob(f"{ep['id']}.*"))
    if existing:
        print(f"[audio] CACHED {existing[0].name}")
        return existing[0]
    out_template = str(AUDIO_DIR / f"{ep['id']}.%(ext)s")
    cmd = [
        YT_DLP,
        "-f",
        "bestaudio",
        "--no-playlist",
        "-o",
        out_template,
        ep["url"],
    ]
    print(f"[audio] FETCH  {ep['id']}  ({ep['title'][:60]})")
    subprocess.run(cmd, check=True)
    after = sorted(AUDIO_DIR.glob(f"{ep['id']}.*"))
    if not after:
        raise SystemExit(f"yt-dlp completed but no audio file for {ep['id']}")
    return after[0]


def transcript_path_for(vid: str) -> Path:
    return TRANSCRIPT_DIR / vid / f"{vid}_full.txt"


def transcribe_audio(vid: str, audio: Path, device: str, compute_type: str) -> Path:
    """Run zoom_transcribe.py on audio; return final transcript path. Idempotent."""
    out_dir = TRANSCRIPT_DIR / vid
    full_out = out_dir / f"{vid}_full.txt"
    if full_out.exists() and full_out.stat().st_size > 0:
        print(f"[txt  ] CACHED {full_out.relative_to(LAB_ROOT)}")
        return full_out
    out_dir.mkdir(parents=True, exist_ok=True)
    cmd = [
        WHISPER_PYTHON,
        str(ZOOM_TRANSCRIBE),
        str(audio),
        "--output-dir",
        str(out_dir),
        "--device",
        device,
        "--compute-type",
        compute_type,
        "--language",
        "en",
        "--initial-prompt",
        PEERAGOGY_PROMPT,
    ]
    print(f"[txt  ] WHISPR {audio.name}")
    subprocess.run(cmd, check=True)
    # zoom_transcribe writes <stem>_full.txt where stem is the input file's stem
    # (the audio file's stem, which is the video id). Match that:
    expected = out_dir / f"{audio.stem}_full.txt"
    if expected != full_out and expected.exists():
        expected.rename(full_out)
    if not full_out.exists():
        # Fallback: search for any *_full.txt under out_dir
        candidates = sorted(out_dir.glob("*_full.txt"))
        if candidates:
            candidates[0].rename(full_out)
    return full_out


def process_episode(ep: dict, manifest: dict, device: str, compute_type: str,
                    transcribe_only: bool) -> None:
    vid = ep["id"]
    record = manifest["episodes"].setdefault(vid, {})
    record.update(
        {
            "index": ep["index"],
            "title": ep["title"],
            "url": ep["url"],
            "duration_seconds": ep["duration"],
        }
    )
    try:
        if transcribe_only:
            audio = audio_path_for(vid)
            if not audio.exists():
                record["status"] = "audio-missing-skipped"
                return
        else:
            audio = download_audio(ep)
        record["audio_path"] = str(audio.relative_to(LAB_ROOT))
        transcript = transcribe_audio(vid, audio, device, compute_type)
        record["transcript_path"] = str(transcript.relative_to(LAB_ROOT))
        record["status"] = "ok"
        record.pop("error", None)
    except subprocess.CalledProcessError as e:
        record["status"] = "error"
        record["error"] = f"{e.cmd[0]} exited {e.returncode}"
        print(f"  ! {ep['id']}: {record['error']}", file=sys.stderr)
    save_manifest(manifest)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--list", action="store_true",
                    help="List the playlist and exit.")
    ap.add_argument("--only", default=None,
                    help="Process only the episode with this YouTube id.")
    ap.add_argument("--transcribe-only", action="store_true",
                    help="Skip download; transcribe whatever audio is already cached.")
    ap.add_argument("--device", default="cpu", choices=["cpu", "cuda"],
                    help="faster-whisper device (default: cpu).")
    ap.add_argument("--compute-type", default="int8",
                    help="faster-whisper compute type (default: int8).")
    args = ap.parse_args()

    if not ZOOM_TRANSCRIBE.exists():
        raise SystemExit(f"zoom_transcribe.py not found at {ZOOM_TRANSCRIBE}")

    print(f"[playlist] discovering episodes via yt-dlp...")
    episodes = list_playlist()
    print(f"[playlist] {len(episodes)} episode(s)")

    if args.list:
        for ep in episodes:
            mins = ep["duration"] // 60
            print(f"  #{ep['index']:>2}  {ep['id']}  {mins:>3}min  {ep['title']}")
        return

    if args.only:
        episodes = [e for e in episodes if e["id"] == args.only]
        if not episodes:
            raise SystemExit(f"No episode with id {args.only}")

    LAB_ROOT.mkdir(parents=True, exist_ok=True)
    manifest = load_manifest()
    for ep in episodes:
        print(f"\n=== #{ep['index']} {ep['id']}  {ep['title']}  "
              f"({ep['duration']//60} min) ===")
        process_episode(ep, manifest, args.device, args.compute_type,
                        args.transcribe_only)
    save_manifest(manifest)
    print(f"\n[done] manifest at {MANIFEST_PATH}")


if __name__ == "__main__":
    main()
