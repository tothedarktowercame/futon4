#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_piano_accordion_harmonica_vocal_forward.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (recorded with piano bleed)
# Track2: piano (recorded with vocal bleed)
# Track3: accordion (overdub)
# Track4: harmonica (overdub)
#
# Vocal-forward variant that ducks the instrument bus with a sidechain
# compressor keyed off the vocal track, plus subtle room ambience.

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (piano) wav}"
t3="${3:?t3 (accordion) wav}"
t4="${4:?t4 (harmonica) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Vocal-forward rough mix with softened top-end and subtle vocal room.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]highpass=f=90,lowpass=f=7000,acompressor=threshold=-22dB:ratio=3:attack=10:release=120,
         equalizer=f=3000:t=q:w=1.0:g=1.2,equalizer=f=5200:t=q:w=1.0:g=-1.2,volume=1.18,
         pan=stereo|c0=c0|c1=c0[vox_dry];
    [vox_dry]asplit=3[vox_sc][vox_dry_mix][vox_fx];
    [vox_sc]apad[vox_sc_pad];
    [vox_fx]aecho=0.65:0.35:45|70:0.10|0.06,highpass=f=140,lowpass=f=4200,volume=0.80[vox_wet];
    [vox_dry_mix][vox_wet]amix=inputs=2:duration=longest:weights='1 0.16':normalize=0[vox_mix];
    [1:a]highpass=f=90, lowpass=f=9000, acompressor=threshold=-22dB:ratio=2:attack=15:release=180,
         volume=1.00, pan=stereo|c0=0.65*c0|c1=0.35*c0[pL];
    [2:a]highpass=f=180,acompressor=threshold=-22dB:ratio=2:attack=10:release=150,
         volume=0.85, pan=stereo|c0=0.45*c0|c1=0.55*c0[aR];
    [3:a]highpass=f=200,acompressor=threshold=-22dB:ratio=2:attack=10:release=150,
         volume=0.80, pan=stereo|c0=0.25*c0|c1=0.75*c0[hR];
    [pL][aR][hR]amix=inputs=3:duration=longest:normalize=0[inst];
    [inst][vox_sc_pad]sidechaincompress=threshold=0.05:ratio=6:attack=15:release=300:makeup=1[duck];
    [vox_mix][duck]amix=inputs=2:duration=longest:normalize=0[m];
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
