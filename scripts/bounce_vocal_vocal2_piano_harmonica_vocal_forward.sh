#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_piano_harmonica_vocal_forward.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal1 (lead)
# Track2: vocal2 (lead)
# Track3: piano (recorded with vocal bleed)
# Track4: harmonica (overdub)
#
# Dual-vocal-forward variant that ducks the instrument bus from a blended
# sidechain key (both leads). The key is softly compressed so simultaneous
# singing does not over-duck piano/harmonica.

t1="${1:?t1 (vocal1) wav}"
t2="${2:?t2 (vocal2) wav}"
t3="${3:?t3 (piano) wav}"
t4="${4:?t4 (harmonica) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Dual-vocal-forward rough mix.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]highpass=f=90,lowpass=f=7600,acompressor=threshold=-23dB:ratio=3:attack=10:release=120,
         equalizer=f=3000:t=q:w=1.0:g=1.2,equalizer=f=5200:t=q:w=1.0:g=-1.0,volume=1.13[v1_raw];
    [1:a]highpass=f=90,lowpass=f=7600,acompressor=threshold=-23dB:ratio=3:attack=10:release=120,
         equalizer=f=3000:t=q:w=1.0:g=1.2,equalizer=f=5200:t=q:w=1.0:g=-1.0,volume=1.13[v2_raw];
    [v1_raw]asplit=2[v1_sc_src][v1_mix_src];
    [v2_raw]asplit=2[v2_sc_src][v2_mix_src];
    [v1_sc_src]pan=mono|c0=c0[v1_sc];
    [v2_sc_src]pan=mono|c0=c0[v2_sc];
    [v1_sc][v2_sc]amix=inputs=2:duration=longest:weights='0.75 0.75':normalize=0,
         acompressor=threshold=-26dB:ratio=2:attack=8:release=160[vox_sc];
    [vox_sc]apad[vox_sc_pad];
    [v1_mix_src]pan=stereo|c0=0.62*c0|c1=0.38*c0[v1_wide];
    [v2_mix_src]pan=stereo|c0=0.38*c0|c1=0.62*c0[v2_wide];
    [v1_wide][v2_wide]amix=inputs=2:duration=longest:normalize=0[vox_mix];
    [2:a]highpass=f=80,lowpass=f=9000,acompressor=threshold=-22dB:ratio=2:attack=18:release=190,
         volume=0.97,pan=stereo|c0=0.65*c0|c1=0.35*c0[pL];
    [3:a]highpass=f=180,lowpass=f=6500,acompressor=threshold=-22dB:ratio=2.2:attack=12:release=170,
         equalizer=f=2900:t=q:w=1.2:g=1.1,volume=0.82,pan=stereo|c0=0.30*c0|c1=0.70*c0[hR];
    [pL][hR]amix=inputs=2:duration=longest:normalize=0[inst];
    [inst][vox_sc_pad]sidechaincompress=threshold=0.06:ratio=4.5:attack=20:release=260:makeup=1[duck];
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
