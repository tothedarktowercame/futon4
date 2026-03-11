#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_guitar_piano_bass.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: guitar (overdub)
# Track3: piano (overdub)
# Track4: bass (overdub)

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (guitar) wav}"
t3="${3:?t3 (piano) wav}"
t4="${4:?t4 (bass) wav}"
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
         equalizer=f=3300:t=q:w=1.0:g=2.6,highshelf=f=7800:g=1.8,
         acompressor=threshold=-20dB:ratio=2.2:attack=15:release=180,volume=1.18,
         pan=stereo|c0=0.56*c0+0.44*c1|c1=0.44*c0+0.56*c1[v];

    [1:a]aformat=channel_layouts=stereo,highpass=f=70,lowpass=f=9000,
         acompressor=threshold=-23dB:ratio=2:attack=14:release=160,volume=0.94,
         pan=stereo|c0=0.72*c0+0.28*c1|c1=0.28*c0+0.72*c1[g];

    [2:a]aformat=channel_layouts=stereo,highpass=f=90,lowpass=f=9500,
         acompressor=threshold=-22dB:ratio=2:attack=15:release=180,volume=0.92,
         pan=stereo|c0=0.38*c0+0.62*c1|c1=0.62*c0+0.38*c1[p];

    [3:a]aformat=channel_layouts=stereo,highpass=f=40,lowpass=f=5000,
         acompressor=threshold=-18dB:ratio=2.4:attack=25:release=250,volume=1.02,
         pan=stereo|c0=0.50*c0+0.50*c1|c1=0.50*c0+0.50*c1[b];

    [v][g][p][b]amix=inputs=4:normalize=0[m];
    [m]alimiter=limit=-1.5dB,aresample=48000
  " \
  -c:a pcm_s16le "$rough"

# 2) Loudness normalize (two-pass) for consistent output.
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
