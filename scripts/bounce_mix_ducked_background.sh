#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_mix_ducked_background.sh fg.wav bg.wav out.wav [out.mp3]
# Mix two existing bounces, with background at 30% volume.

fg="${1:?foreground wav}"
bg="${2:?background wav}"
out="${3:?output wav}"
out_mp3="${4:-}"

ffmpeg -hide_banner -y \
  -i "$fg" -i "$bg" \
  -filter_complex "
    [1:a]volume=0.30[bg];
    [0:a][bg]amix=inputs=2:normalize=0[m];
    [m]alimiter=limit=-1.5dB,aresample=48000
  " \
  -c:a pcm_s16le "$out"

echo "Wrote: $out"

if [ -n "$out_mp3" ]; then
  ffmpeg -hide_banner -y -i "$out" -c:a libmp3lame -b:a 192k "$out_mp3"
  echo "Wrote: $out_mp3"
fi
