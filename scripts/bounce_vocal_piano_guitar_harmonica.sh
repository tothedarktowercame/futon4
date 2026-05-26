#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_piano_guitar_harmonica.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: piano
# Track3: guitar
# Track4: harmonica
#
# Designed for tracks where extra vocals may be bleeding into the
# instrumental lanes. The instrument lanes keep a low HPF and a soft
# lowpass so hidden vocal content survives, and the sidechain duck is
# keyed off the main vocal only (not a blended key) so bleed vocals
# stay audible during the lead's silent moments instead of being
# crushed by a key they didn't sing into. Reciprocal -1.5 dB notches
# at 1.8/2.2/3.0 kHz across piano/guitar/harmonica give the lead room.

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (piano) wav}"
t3="${3:?t3 (guitar) wav}"
t4="${4:?t4 (harmonica) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Vocal-forward rough mix; instrument lanes preserve possible bleed vocals.
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

    [1:a]aformat=channel_layouts=stereo,highpass=f=60,lowpass=f=11000,
         equalizer=f=1800:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-22dB:ratio=2:attack=18:release=200,
         volume=0.95,
         pan=stereo|c0=0.55*c0+0.45*c1|c1=0.45*c0+0.55*c1[piano];

    [2:a]aformat=channel_layouts=mono,highpass=f=70,lowpass=f=10000,
         equalizer=f=2200:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-20dB:ratio=2.2:attack=18:release=200,
         volume=0.95,
         pan=stereo|c0=0.65*c0|c1=0.35*c0[guitar];

    [3:a]aformat=channel_layouts=mono,highpass=f=200,lowpass=f=8500,
         equalizer=f=3000:t=q:w=1.2:g=-1.5,
         highshelf=f=7000:g=-1.0,
         acompressor=threshold=-22dB:ratio=2.2:attack=12:release=180,
         volume=0.85,
         pan=stereo|c0=0.35*c0|c1=0.65*c0[harm];

    [piano][guitar][harm]amix=inputs=3:duration=longest:normalize=0[inst];

    [inst][v_sc_pad]sidechaincompress=threshold=0.07:ratio=3.0:attack=22:release=280:makeup=1[inst_duck];

    [v_w][inst_duck]amix=inputs=2:duration=longest:normalize=0[m];
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
