#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./bounce_vocal_drum_bass_guitar_vocal_forward_sharp_drums.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
# Track1: vocal (lead, slightly low in the raw recording)
# Track2: drum (kit or percussion stem)
# Track3: bass (overdub)
# Track4: guitar (overdub)
#
# Vocal-forward variant with a stronger lead push, sharper drums, and a
# background bus that yields more decisively to the lyric instead of clouding it.

t1="${1:?t1 (vocal) wav}"
t2="${2:?t2 (drum) wav}"
t3="${3:?t3 (bass) wav}"
t4="${4:?t4 (guitar) wav}"
out="${5:?output wav}"
out_mp3="${6:-}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

rough="$tmp/roughmix.wav"
json="$tmp/loudnorm.json"

# 1) Vocal-forward rough mix with the bass/guitar bus ducked harder than the drums,
# plus a first-minute center-weighted rescue lane for backing vocals on the guitar stem.
ffmpeg -hide_banner -y \
  -i "$t1" -i "$t2" -i "$t3" -i "$t4" \
  -filter_complex "
    [0:a]aformat=channel_layouts=stereo,highpass=f=95,lowpass=f=14000,
         equalizer=f=220:t=q:w=1.2:g=-1.8,
         equalizer=f=2500:t=q:w=1.0:g=4.2,
         equalizer=f=4300:t=q:w=1.0:g=3.0,
         highshelf=f=8200:g=2.4,
         acompressor=threshold=-24dB:ratio=3.6:attack=8:release=150,
         volume=2.38,
         pan=stereo|c0=0.70*c0+0.30*c1|c1=0.30*c0+0.70*c1[vox];

    [vox]asplit=3[vox_sc][vox_dry][vox_fx];
    [vox_sc]asplit=3[vox_sc_bg_src][vox_sc_dr_src][vox_sc_gv_src];
    [vox_sc_bg_src]apad[vox_sc_bg];
    [vox_sc_dr_src]apad[vox_sc_dr];
    [vox_sc_gv_src]apad[vox_sc_gv];
    [vox_fx]aecho=0.72:0.26:45|95:0.10|0.06,
         highpass=f=180,lowpass=f=5600,volume=0.82[vox_wet];
    [vox_dry][vox_wet]amix=inputs=2:weights='1 0.16':duration=longest:normalize=0[vox_mix];

    [1:a]aformat=channel_layouts=stereo,highpass=f=65,lowpass=f=14500,
         equalizer=f=250:t=q:w=1.1:g=-3.0,
         equalizer=f=4300:t=q:w=1.0:g=3.4,
         highshelf=f=9200:g=2.0,
         acompressor=threshold=-20dB:ratio=2.5:attack=4:release=85,
         alimiter=limit=0.72:attack=2:release=55:level=disabled,
         volume=0.40,
         pan=stereo|c0=c0|c1=c1[dr];

    [2:a]aformat=channel_layouts=stereo,highpass=f=45,lowpass=f=2600,
         equalizer=f=180:t=q:w=1.1:g=-1.6,
         equalizer=f=320:t=q:w=1.0:g=-3.2,
         acompressor=threshold=-20dB:ratio=3.0:attack=18:release=180,
         volume=0.60,
         pan=stereo|c0=0.50*c0+0.50*c1|c1=0.50*c0+0.50*c1[ba];

    [3:a]aformat=channel_layouts=stereo,highpass=f=130,lowpass=f=9800,
         equalizer=f=320:t=q:w=1.0:g=-3.0,
         equalizer=f=1800:t=q:w=1.0:g=1.4,
         equalizer=f=3200:t=q:w=1.0:g=1.8,
         highshelf=f=6500:g=1.2,
         acompressor=threshold=-24dB:ratio=2.0:attack=12:release=150[g_src];
    [g_src]asplit=2[g_base_src][g_vox_src];

    [g_base_src]volume='if(lt(t,60),0.84,0.66)',
         pan=stereo|c0=0.84*c0+0.16*c1|c1=0.16*c0+0.84*c1[gt];
    [g_vox_src]highpass=f=170,lowpass=f=7200,
         equalizer=f=2400:t=q:w=1.0:g=3.6,
         equalizer=f=4200:t=q:w=1.0:g=2.2,
         highshelf=f=7600:g=1.5,
         acompressor=threshold=-26dB:ratio=3.6:attack=6:release=140,
         volume='if(lt(t,60),2.18,0.60)',
         pan=stereo|c0=0.72*c0+0.28*c1|c1=0.28*c0+0.72*c1[gvox];

    [ba][gt]amix=inputs=2:duration=longest:normalize=0,
         highpass=f=55,lowpass=f=9200,
         equalizer=f=260:t=q:w=1.0:g=-1.8[bg];
    [bg][vox_sc_bg]sidechaincompress=threshold=0.018:ratio=10:attack=4:release=230:makeup=1[bg_duck];
    [dr][vox_sc_dr]sidechaincompress=threshold=0.040:ratio=2.6:attack=4:release=170:makeup=1[dr_duck];
    [gvox][vox_sc_gv]sidechaincompress=threshold=0.040:ratio=2.4:attack=6:release=180:makeup=1[gvox_duck];
    [vox_mix][gvox_duck][dr_duck][bg_duck]amix=inputs=4:weights='1.10 0.78 0.58 0.44':duration=longest:normalize=0[m];
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
