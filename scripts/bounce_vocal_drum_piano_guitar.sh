#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_drum_piano_guitar.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead)
# Track2: drum
# Track3: piano
# Track4: guitar
#
# Vocal-forward mix with no bass — piano picks up the low-mid weight.
# Drums pass through at full level (slow-attack glue comp preserves
# transients, light air at 5 kHz). Piano + guitar form a harmonic bus
# that gets sidechain-ducked from the vocal key, so the backing parts
# get out of the lead's way without flattening the kit's punch.

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (drum) wav}"
t3="${3:?t3 (piano) wav}"
t4="${4:?t4 (guitar) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Vocal-forward rough mix; drums bypass the duck, harmonic bus is ducked.
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

    [1:a]aformat=channel_layouts=stereo,highpass=f=30,
         acompressor=threshold=-18dB:ratio=2.5:attack=25:release=250,
         equalizer=f=5000:t=q:w=1.4:g=1.0,
         volume=1.00[drums];

    [2:a]aformat=channel_layouts=mono,highpass=f=50,lowpass=f=11000,
         equalizer=f=2000:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-22dB:ratio=2:attack=18:release=200,
         volume=0.96,
         pan=stereo|c0=0.62*c0|c1=0.38*c0[piano];

    [3:a]aformat=channel_layouts=mono,highpass=f=70,lowpass=f=10000,
         equalizer=f=2500:t=q:w=1.4:g=-1.5,
         acompressor=threshold=-20dB:ratio=2.2:attack=18:release=200,
         volume=0.95,
         pan=stereo|c0=0.38*c0|c1=0.62*c0[guitar];

    [piano][guitar]amix=inputs=2:duration=longest:normalize=0[harm];

    [harm][v_sc_pad]sidechaincompress=threshold=0.07:ratio=3.5:attack=20:release=260:makeup=1[harm_duck];

    [v_w][drums][harm_duck]amix=inputs=3:duration=longest:normalize=0[m];
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
