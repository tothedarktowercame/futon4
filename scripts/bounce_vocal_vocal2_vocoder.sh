#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_vocoder.sh t1.wav t2.wav out.wav [out.mp3]
# Track1: vocal1 (lyrics / modulator)
# Track2: vocal2 (carrier stand-in)
#
# This is an experimental pseudo-vocoder stage, not a classic band vocoder.
# It first builds a hidden pitch-matched shadow copy of vocal1 aligned toward
# vocal2, then uses vocal1 to shape, gate, and sidechain a heavily conditioned
# vocal2 carrier while the shadow track supplies the synthetic lock.

t1="${1:?t1 (vocal1) wav}"
t2="${2:?t2 (vocal2) wav}"
out="${3:?output wav}"
out_mp3="${4:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"
shadow="$tmp/vocal1_shadow.wav"

python3 /home/joe/code/futon4/scripts/vocal_vocal2_shadow.py \
  --source "$t1" \
  --target "$t2" \
  --output "$shadow" \
  --mix 1.0 \
  --max-shift-semitones 14 \
  --smoothing-kernel 5

# 1) Build a pseudo-vocoder rough mix from modulator + carrier + shadow.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$shadow" \
  -filter_complex "
    [0:a]aformat=channel_layouts=stereo,highpass=f=90,lowpass=f=7600,
         acompressor=threshold=-22dB:ratio=2.8:attack=10:release=140,
         equalizer=f=2600:t=q:w=1.0:g=2.0,equalizer=f=5200:t=q:w=1.2:g=-1.2,
         volume=1.05[vox_pre];
    [vox_pre]asplit=2[vox_dry_src][vox_key_src];
    [vox_dry_src]pan=stereo|c0=0.52*c0+0.48*c1|c1=0.48*c0+0.52*c1,
         volume=0.20[vox_dry];
    [vox_key_src]pan=mono|c0=0.5*c0+0.5*c1,highpass=f=140,lowpass=f=4300,
         acompressor=threshold=-28dB:ratio=3.5:attack=5:release=90,
         volume=1.80[vox_key];
    [vox_key]asplit=3[vox_key_pad_src][vox_key_gate1_src][vox_key_gate2_src];
    [vox_key_pad_src]apad[vox_key_pad];
    [vox_key_gate1_src]apad[vox_key_gate1];
    [vox_key_gate2_src]apad[vox_key_gate2];

    [1:a]aformat=channel_layouts=stereo,highpass=f=120,lowpass=f=6900,
         acompressor=threshold=-26dB:ratio=3.2:attack=8:release=140,
         equalizer=f=900:t=q:w=1.0:g=1.0,equalizer=f=2300:t=q:w=1.0:g=2.4,
         equalizer=f=3400:t=q:w=1.0:g=1.4,volume=1.08[carrier_pre];
    [carrier_pre]asplit=2[carrier_mix_src][carrier_mult_src];
    [carrier_mix_src]pan=stereo|c0=0.58*c0+0.42*c1|c1=0.42*c0+0.58*c1[carrier_mix];
    [carrier_mix][vox_key_gate1]sidechaingate=threshold=0.014:ratio=18:attack=8:release=260[carrier_gate];
    [carrier_gate][vox_key_pad]sidechaincompress=threshold=0.035:ratio=9:attack=3:release=120:makeup=1.15[carrier_duck];
    [carrier_mult_src]pan=mono|c0=0.5*c0+0.5*c1,bandpass=f=1500:w=2600,
         acompressor=threshold=-28dB:ratio=3:attack=5:release=110,
         volume=1.25[carrier_mult];

    [2:a]aformat=channel_layouts=stereo,highpass=f=140,lowpass=f=4600,
         acompressor=threshold=-30dB:ratio=3.5:attack=4:release=90,
         equalizer=f=2100:t=q:w=1.1:g=2.0,volume=0.95[shadow_pre];
    [shadow_pre]asplit=2[shadow_mix_src][shadow_mult_src];
    [shadow_mix_src]pan=stereo|c0=0.46*c0|c1=0.54*c0,volume=0.52[shadow_mix];
    [shadow_mult_src]pan=mono|c0=0.5*c0+0.5*c1,bandpass=f=1700:w=2300,
         acompressor=threshold=-31dB:ratio=4:attack=4:release=100,
         volume=1.55[shadow_mult];

    [carrier_mult][shadow_mult]amultiply,
         highpass=f=260,lowpass=f=4200,
         acompressor=threshold=-27dB:ratio=3.5:attack=4:release=100,
         volume=0.52,pan=stereo|c0=0.47*c0|c1=0.53*c0[robot_raw];
    [robot_raw][vox_key_gate2]sidechaingate=threshold=0.010:ratio=14:attack=6:release=240[robot];

    [carrier_duck][robot][vox_dry][shadow_mix]amix=inputs=4:duration=longest:weights='0.82 1.00 0.40 1.00':normalize=0[m];
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
