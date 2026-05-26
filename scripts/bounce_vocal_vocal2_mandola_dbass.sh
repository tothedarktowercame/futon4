#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_mandola_dbass.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: vocal2 (harmony)
# Track3: mandola (with reverb)
# Track4: double bass
#
# Lead-plus-harmony arrangement with mandola and upright bass.
# - Lead vocal gets full presence boost; harmony gets a smaller
#   presence boost (+1.2 dB vs +2 dB) and slight R pan so the two
#   voices don't double-clash at 2.8 kHz.
# - Mandola passes through a chained-aecho plate-room reverb send
#   with a 6.5 kHz wet lowpass, mixed at ~32% wet.
# - Double bass holds the low end with the upright EQ profile
#   (+1 dB @ 90, -1 dB @ 350, lowpass 4500) and passes through
#   the duck untouched.
# - Sidechain key is a blended/compressed mix of both vocals,
#   ducking only the (wet) mandola.

t1="${1:?t1 (vocal lead) wav}"
t2="${2:?t2 (vocal harmony) wav}"
t3="${3:?t3 (mandola) wav}"
t4="${4:?t4 (double bass) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Lead+harmony rough mix; mandola gets aecho plate-room send; dbass unducked.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]aformat=channel_layouts=mono,highpass=f=80,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2800:t=q:w=1.0:g=2.0,
         equalizer=f=6500:t=q:w=1.4:g=-1.0,
         volume=1.18[v1_pre];

    [1:a]aformat=channel_layouts=mono,highpass=f=90,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2800:t=q:w=1.0:g=1.2,
         equalizer=f=6500:t=q:w=1.4:g=-1.0,
         volume=0.95[v2_pre];

    [v1_pre]asplit=2[v1_sc][v1_mix];
    [v2_pre]asplit=2[v2_sc][v2_mix];

    [v1_sc][v2_sc]amix=inputs=2:duration=longest:weights='0.75 0.75':normalize=0,
         acompressor=threshold=-26dB:ratio=2:attack=8:release=160[vox_sc];
    [vox_sc]apad[vox_sc_pad];

    [v1_mix]pan=stereo|c0=0.50*c0|c1=0.50*c0[v1_w];
    [v2_mix]pan=stereo|c0=0.40*c0|c1=0.60*c0[v2_w];

    [2:a]aformat=channel_layouts=mono,highpass=f=100,lowpass=f=10000,
         equalizer=f=2000:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-22dB:ratio=2:attack=18:release=200,
         volume=0.92,
         pan=stereo|c0=0.62*c0|c1=0.38*c0[mand_pre];

    [mand_pre]asplit=2[mand_dry][mand_send];
    [mand_send]aecho=in_gain=0.8:out_gain=0.9:delays=43|97|163|239:decays=0.5|0.4|0.3|0.2,
               aecho=in_gain=0.8:out_gain=0.9:delays=11|17:decays=0.55|0.45,
               lowpass=f=6500[mand_wet];
    [mand_dry][mand_wet]amix=inputs=2:duration=longest:weights='1.0 0.32':normalize=0[mand_full];

    [3:a]aformat=channel_layouts=mono,highpass=f=35,lowpass=f=4500,
         equalizer=f=90:t=q:w=1.2:g=1.0,
         equalizer=f=350:t=q:w=1.4:g=-1.0,
         acompressor=threshold=-18dB:ratio=2.5:attack=25:release=250,
         volume=1.05,
         pan=stereo|c0=0.50*c0|c1=0.50*c0[dbass];

    [mand_full][vox_sc_pad]sidechaincompress=threshold=0.07:ratio=3.5:attack=20:release=260:makeup=1[mand_duck];

    [v1_w][v2_w][mand_duck][dbass]amix=inputs=4:duration=longest:normalize=0[m];
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
