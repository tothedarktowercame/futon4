#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_bass_pbass_accordion.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocals (recorded with bass bleed)
# Track2: bass (recorded with vocal bleed)
# Track3: pbass (overdub)
# Track4: accordion (overdub)

t1="${1:?t1 (vocals) wav}"
t2="${2:?t2 (bass) wav}"
t3="${3:?t3 (pbass) wav}"
t4="${4:?t4 (accordion) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Rough mix with very gentle per-track conditioning.
#    Keep it light because Track1/2 were captured together (bleed).
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]highpass=f=80,  acompressor=threshold=-20dB:ratio=2:attack=20:release=200, volume=1.10, pan=stereo|c0=c0|c1=c0[v];
    [1:a]highpass=f=100, acompressor=threshold=-22dB:ratio=2:attack=15:release=180, volume=0.95, pan=stereo|c0=c0|c1=0*c0[bL];
    [2:a]highpass=f=30,  lowpass=f=2500, acompressor=threshold=-18dB:ratio=2.5:attack=25:release=250, volume=1.05, pan=stereo|c0=c0|c1=c0[pb];
    [3:a]highpass=f=180, acompressor=threshold=-22dB:ratio=2:attack=10:release=150, volume=0.90, pan=stereo|c0=0*c0|c1=c0[aR];
    [v][bL][pb][aR]amix=inputs=4:normalize=0[m];
    [m]alimiter=limit=-1.5dB,aresample=48000
  " \
  -c:a pcm_s16le "$rough"

# 2) Loudness normalize (two-pass) for consistent "listenable" output.
# Pass 1: analysis -> JSON to stderr
ffmpeg -hide_banner -y -i "$rough" \
  -af "loudnorm=I=-16:TP=-1.5:LRA=11:print_format=json" \
  -f null - 2> "$json"

# Extract the measured values (no jq needed; uses sed).
measured_I=$(sed -n 's/.*"input_i"[ ]*:[ ]*"\{0,1\}\([-0-9.]*\)".*/\1/p' "$json" | tail -n1)
measured_TP=$(sed -n 's/.*"input_tp"[ ]*:[ ]*"\{0,1\}\([-0-9.]*\)".*/\1/p' "$json" | tail -n1)
measured_LRA=$(sed -n 's/.*"input_lra"[ ]*:[ ]*"\{0,1\}\([-0-9.]*\)".*/\1/p' "$json" | tail -n1)
measured_thresh=$(sed -n 's/.*"input_thresh"[ ]*:[ ]*"\{0,1\}\([-0-9.]*\)".*/\1/p' "$json" | tail -n1)
offset=$(sed -n 's/.*"target_offset"[ ]*:[ ]*"\{0,1\}\([-0-9.]*\)".*/\1/p' "$json" | tail -n1)

ffmpeg -hide_banner -y -i "$rough" \
  -af "loudnorm=I=-16:TP=-1.5:LRA=11:measured_I=${measured_I}:measured_TP=${measured_TP}:measured_LRA=${measured_LRA}:measured_thresh=${measured_thresh}:offset=${offset}:linear=true" \
  -c:a pcm_s16le "$out"

echo "Wrote: $out"

if [ -n "$out_mp3" ]; then
  ffmpeg -hide_banner -y -i "$out" -c:a libmp3lame -b:a 192k "$out_mp3"
  echo "Wrote: $out_mp3"
fi
