#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   FX=reverb ./bounce_vocal_bass_arco_fx.sh t1.wav t2.wav out.wav [out.mp3]
#   FX=distort ./bounce_vocal_bass_arco_fx.sh t1.wav t2.wav out.wav [out.mp3]
#   FX=both ./bounce_vocal_bass_arco_fx.sh t1.wav t2.wav out.wav [out.mp3]
#   FX=none ./bounce_vocal_bass_arco_fx.sh t1.wav t2.wav out.wav [out.mp3]
#
# Track1: vocals
# Track2: bass (arco)
#
# Default FX is reverb if FX is unset.

t1="${1:?t1 (vocals) wav}"
t2="${2:?t2 (bass arco) wav}"
out="${3:?output wav}"
out_mp3="${4:-}"

fx="${FX:-reverb}"

case "$fx" in
  reverb)
    bass_fx=",aecho=in_gain=0.85:out_gain=0.6:delays=55|110:decays=0.25|0.15"
    ;;
  distort)
    bass_fx=",volume=1.6,asoftclip=type=tanh:threshold=0.75:output=0.9:param=1.2:oversample=4,volume=0.9"
    ;;
  both)
    bass_fx=",volume=1.6,asoftclip=type=tanh:threshold=0.75:output=0.9:param=1.2:oversample=4,volume=0.9,aecho=in_gain=0.85:out_gain=0.6:delays=55|110:decays=0.22|0.12"
    ;;
  none)
    bass_fx=""
    ;;
  *)
    echo "Unknown FX='$fx' (use reverb|distort|both|none)" >&2
    exit 1
    ;;
 esac

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Rough mix with light per-track conditioning.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" \
  -filter_complex "
    [0:a]highpass=f=80,  acompressor=threshold=-20dB:ratio=2:attack=20:release=200,
         equalizer=f=3200:t=q:w=1.0:g=2, volume=1.10, pan=stereo|c0=c0|c1=c0[v];
    [1:a]highpass=f=35,  lowpass=f=3500, acompressor=threshold=-18dB:ratio=2.8:attack=25:release=260,
         equalizer=f=140:t=q:w=1.0:g=2, volume=1.05${bass_fx}, pan=stereo|c0=c0|c1=c0[ba];
    [v][ba]amix=inputs=2:normalize=0[m];
    [m]alimiter=limit=-1.5dB,aresample=48000
  " \
  -c:a pcm_s16le "$rough"

# 2) Loudness normalize (two-pass) for consistent "listenable" output.
ffmpeg -hide_banner -y -i "$rough" \
  -af "loudnorm=I=-16:TP=-1.5:LRA=11:print_format=json" \
  -f null - 2> "$json"

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
