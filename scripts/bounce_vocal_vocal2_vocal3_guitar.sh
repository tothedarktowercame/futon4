#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_vocal3_guitar.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal1 (co-equal, panned left)
# Track2: vocal2 (co-equal, centre)
# Track3: vocal3 (co-equal, panned right)
# Track4: guitar
#
# Co-equal vocal trio with reciprocal EQ carving: each voice owns one
# presence slot (2.5/4/5.5 kHz) and gets a -1 dB notch at the other two
# slots so the three voices stop competing for the same air. HPF kept low
# so piano bleed inside the vocal tracks survives as room glue. Guitar is
# notched at 1.8 kHz to give the stack room and gently ducked from a
# blended sidechain key built from all three voices.

t1="${1:?t1 (vocal1) wav}"
t2="${2:?t2 (vocal2) wav}"
t3="${3:?t3 (vocal3) wav}"
t4="${4:?t4 (guitar) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Co-equal trio rough mix with reciprocal EQ slots.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]aformat=channel_layouts=mono,highpass=f=70,lowpass=f=9500,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2500:t=q:w=1.0:g=2.0,
         equalizer=f=4000:t=q:w=1.2:g=-1.0,
         equalizer=f=5500:t=q:w=1.2:g=-1.0,
         volume=1.10[v1_pre];

    [1:a]aformat=channel_layouts=mono,highpass=f=70,lowpass=f=9500,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2500:t=q:w=1.2:g=-1.0,
         equalizer=f=4000:t=q:w=1.0:g=2.0,
         equalizer=f=5500:t=q:w=1.2:g=-1.0,
         volume=1.10[v2_pre];

    [2:a]aformat=channel_layouts=mono,highpass=f=70,lowpass=f=9500,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2500:t=q:w=1.2:g=-1.0,
         equalizer=f=4000:t=q:w=1.2:g=-1.0,
         equalizer=f=5500:t=q:w=1.0:g=2.0,
         volume=1.10[v3_pre];

    [v1_pre]asplit=2[v1_sc][v1_mix];
    [v2_pre]asplit=2[v2_sc][v2_mix];
    [v3_pre]asplit=2[v3_sc][v3_mix];

    [v1_sc][v2_sc][v3_sc]amix=inputs=3:duration=longest:weights='0.6 0.6 0.6':normalize=0,
         acompressor=threshold=-26dB:ratio=2.2:attack=8:release=180[vox_sc];
    [vox_sc]apad[vox_sc_pad];

    [v1_mix]pan=stereo|c0=0.78*c0|c1=0.22*c0[v1_w];
    [v2_mix]pan=stereo|c0=0.50*c0|c1=0.50*c0[v2_w];
    [v3_mix]pan=stereo|c0=0.22*c0|c1=0.78*c0[v3_w];

    [v1_w][v2_w][v3_w]amix=inputs=3:duration=longest:normalize=0[vox_mix];

    [3:a]aformat=channel_layouts=stereo,highpass=f=80,lowpass=f=10000,
         equalizer=f=1800:t=q:w=1.5:g=-2.0,
         acompressor=threshold=-20dB:ratio=2.2:attack=18:release=200,
         volume=0.92,
         pan=stereo|c0=0.55*c0+0.45*c1|c1=0.45*c0+0.55*c1[gtr];

    [gtr][vox_sc_pad]sidechaincompress=threshold=0.07:ratio=3.5:attack=20:release=260:makeup=1[gtr_duck];

    [vox_mix][gtr_duck]amix=inputs=2:duration=longest:normalize=0[m];
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
