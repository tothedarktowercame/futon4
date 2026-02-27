#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_bass_accordion.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: vocal2 (overdub)
# Track3: bass (overdub)
# Track4: accordion (overdub)

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (vocal2) wav}"
t3="${3:?t3 (bass) wav}"
t4="${4:?t4 (accordion) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Rough mix with gentle per-track conditioning.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]aformat=channel_layouts=stereo,highpass=f=80,
         equalizer=f=3500:t=q:w=1.0:g=3, highshelf=f=8000:g=2,
         acompressor=threshold=-20dB:ratio=2.2:attack=15:release=180, volume=1.25,
         pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[v1];

    [1:a]aformat=channel_layouts=stereo,highpass=f=100,
         equalizer=f=3200:t=q:w=1.0:g=2.5, highshelf=f=7500:g=1.5,
         acompressor=threshold=-22dB:ratio=2.2:attack=15:release=180, volume=0.95,
         pan=stereo|c0=0.6*c0+0.4*c1|c1=0.4*c0+0.6*c1[v2];

    [2:a]aformat=channel_layouts=stereo,highpass=f=40,  lowpass=f=5000, acompressor=threshold=-18dB:ratio=2.5:attack=25:release=250, volume=1.05,
         pan=stereo|c0=0.7*c0+0.3*c1|c1=0.3*c0+0.7*c1[ba];

    [3:a]aformat=channel_layouts=stereo,highpass=f=180, acompressor=threshold=-22dB:ratio=2:attack=10:release=150, volume=0.80,
         pan=stereo|c0=0.8*c0+0.2*c1|c1=0.2*c0+0.8*c1[acc];

    [v1][v2][ba][acc]amix=inputs=4:normalize=0[m];
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
