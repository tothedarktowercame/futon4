#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_vocal2_shadow_variants.sh vocal1.wav vocal2.wav out-dir [stem]
#
# Builds a warped shadow voice from vocal1->vocal2 and renders a few
# listenable variants for quick audition:
#   - raw: the matched shadow track
#   - clean: a more intelligible centered shadow voice
#   - demonic: darker/compressed/echoed version
#   - choir: a widened, lightly detuned stack

t1="${1:?t1 (vocal1) wav}"
t2="${2:?t2 (vocal2) wav}"
out_dir="${3:?output directory}"
stem="${4:-shadow-voice}"

mkdir -p "$out_dir"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

shadow="$tmp/${stem}-raw.wav"
raw_out="${out_dir}/${stem}-raw.wav"
raw_mp3="${out_dir}/${stem}-raw.mp3"
clean_out="${out_dir}/${stem}-clean.wav"
clean_mp3="${out_dir}/${stem}-clean.mp3"
demonic_out="${out_dir}/${stem}-demonic.wav"
demonic_mp3="${out_dir}/${stem}-demonic.mp3"
choir_out="${out_dir}/${stem}-choir.wav"
choir_mp3="${out_dir}/${stem}-choir.mp3"

python3 /home/joe/code/futon4/scripts/vocal_vocal2_shadow.py \
  --source "$t1" \
  --target "$t2" \
  --output "$shadow" \
  --mix 1.0 \
  --max-shift-semitones 14 \
  --smoothing-kernel 5

cp "$shadow" "$raw_out"
ffmpeg -hide_banner -y -i "$raw_out" -c:a libmp3lame -b:a 192k "$raw_mp3"

ffmpeg -hide_banner -y \
  -i "$shadow" \
  -af "highpass=f=120,lowpass=f=3600,acompressor=threshold=-30dB:ratio=3.2:attack=6:release=110,equalizer=f=1100:t=q:w=1.0:g=1.4,equalizer=f=2300:t=q:w=1.0:g=2.2,volume=2.1,alimiter=limit=-1.5dB,aresample=48000" \
  -c:a pcm_s16le "$clean_out"
ffmpeg -hide_banner -y -i "$clean_out" -c:a libmp3lame -b:a 192k "$clean_mp3"

ffmpeg -hide_banner -y \
  -i "$shadow" \
  -af "highpass=f=180,lowpass=f=3200,acompressor=threshold=-32dB:ratio=4:attack=5:release=120,equalizer=f=900:t=q:w=1.0:g=2.5,equalizer=f=2100:t=q:w=1.0:g=3.0,aecho=0.75:0.55:45:0.20,volume=2.4,alimiter=limit=-1.5dB,aresample=48000" \
  -c:a pcm_s16le "$demonic_out"
ffmpeg -hide_banner -y -i "$demonic_out" -c:a libmp3lame -b:a 192k "$demonic_mp3"

ffmpeg -hide_banner -y \
  -i "$shadow" \
  -filter_complex "
    [0:a]highpass=f=140,lowpass=f=3800,acompressor=threshold=-30dB:ratio=3.5:attack=6:release=120,volume=1.55[base];
    [0:a]rubberband=pitch=0.977,highpass=f=140,lowpass=f=3800,adelay=18|18,volume=1.05,pan=stereo|c0=0.78*c0|c1=0.22*c0[left];
    [0:a]rubberband=pitch=1.024,highpass=f=140,lowpass=f=3800,adelay=34|34,volume=1.00,pan=stereo|c0=0.22*c0|c1=0.78*c0[right];
    [base]pan=stereo|c0=0.55*c0|c1=0.55*c0[center];
    [center][left][right]amix=inputs=3:normalize=0,alimiter=limit=-1.5dB,aresample=48000
  " \
  -c:a pcm_s16le "$choir_out"
ffmpeg -hide_banner -y -i "$choir_out" -c:a libmp3lame -b:a 192k "$choir_mp3"

printf 'Wrote:\n'
printf '  %s\n' "$raw_out" "$raw_mp3" "$clean_out" "$clean_mp3" "$demonic_out" "$demonic_mp3" "$choir_out" "$choir_mp3"
