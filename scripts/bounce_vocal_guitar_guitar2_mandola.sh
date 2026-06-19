#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_guitar_guitar2_mandola.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: guitar (rhythm)
# Track3: guitar2 (lead/topline)
# Track4: mandola (with reverb)
#
# All-strings arrangement (no rhythm section). Guitar1 takes the
# rhythm/low-body role (HPF 60) and pans ~60% L; guitar2 takes the
# lead/topline role (HPF 90, +0.5 dB @ 4 kHz presence) and pans
# ~60% R. Reciprocal -1 dB notches at 2.5/1.5 kHz across the
# guitars and -1.5 dB @ 2 kHz on the mandola give the vocal room.
# Mandola passes through the same chained-aecho plate-room reverb
# send as the mandola+dbass script. The full string bus is
# sidechain-ducked from the vocal key.

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (guitar rhythm) wav}"
t3="${3:?t3 (guitar2 lead) wav}"
t4="${4:?t4 (mandola) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Vocal-forward rough mix; full string bus is ducked together.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]aformat=channel_layouts=mono,highpass=f=80,
         acompressor=threshold=-22dB:ratio=2.4:attack=12:release=160,
         equalizer=f=2800:t=q:w=1.0:g=2.0,
         equalizer=f=6500:t=q:w=1.4:g=-1.0,
         volume=1.18[v_pre];

    [v_pre]asplit=2[v_sc][v_mix];
    [v_sc]acompressor=threshold=-26dB:ratio=2:attack=8:release=160[v_sc_c];
    [v_sc_c]apad[v_sc_pad];
    [v_mix]pan=stereo|c0=0.50*c0|c1=0.50*c0[v_w];

    [1:a]aformat=channel_layouts=mono,highpass=f=60,lowpass=f=10000,
         equalizer=f=2500:t=q:w=1.4:g=-1.0,
         acompressor=threshold=-20dB:ratio=2.2:attack=18:release=200,
         volume=0.95,
         pan=stereo|c0=0.78*c0|c1=0.22*c0[g1];

    [2:a]aformat=channel_layouts=mono,highpass=f=90,lowpass=f=10500,
         equalizer=f=1500:t=q:w=1.4:g=-1.0,
         equalizer=f=4000:t=q:w=1.2:g=0.5,
         acompressor=threshold=-20dB:ratio=2.2:attack=18:release=200,
         volume=0.92,
         pan=stereo|c0=0.22*c0|c1=0.78*c0[g2];

    [3:a]aformat=channel_layouts=mono,highpass=f=100,lowpass=f=10000,
         equalizer=f=2000:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-22dB:ratio=2:attack=18:release=200,
         volume=0.92,
         pan=stereo|c0=0.62*c0|c1=0.38*c0[mand_pre];

    [mand_pre]asplit=2[mand_dry][mand_send];
    [mand_send]aecho=in_gain=0.8:out_gain=0.9:delays=43|97|163|239:decays=0.5|0.4|0.3|0.2,
               aecho=in_gain=0.8:out_gain=0.9:delays=11|17:decays=0.55|0.45,
               lowpass=f=6500[mand_wet];
    [mand_dry][mand_wet]amix=inputs=2:duration=longest:weights='1.0 0.32':normalize=0[mand_full];

    [g1][g2][mand_full]amix=inputs=3:duration=longest:normalize=0[strings];

    [strings][v_sc_pad]sidechaincompress=threshold=0.07:ratio=3.5:attack=20:release=260:makeup=1[strings_duck];

    [v_w][strings_duck]amix=inputs=2:duration=longest:normalize=0[m];
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
