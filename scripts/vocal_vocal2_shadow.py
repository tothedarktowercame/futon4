#!/usr/bin/env python3
"""Build a pitch-matched shadow vocal for experimental dual-vocal vocoder work."""

from __future__ import annotations

import argparse
import math
import subprocess
import tempfile
from pathlib import Path

import numpy as np
from scipy.io import wavfile
from scipy.signal import butter, correlate, correlation_lags, filtfilt, medfilt, resample


DEFAULT_SR = 48_000
FRAME_LENGTH = 2048
HOP_LENGTH = 256
MIN_F0 = 80.0
MAX_F0 = 500.0
MIN_VOICED_CORR = 0.35
MIN_RMS = 0.002
DEFAULT_MAX_SHIFT_SEMITONES = 14.0
DEFAULT_SMOOTHING_KERNEL = 5
DEFAULT_MAX_LAG_MS = 220.0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create a pitch-matched shadow copy of vocal1 using vocal2 as target contour."
    )
    parser.add_argument("--source", required=True, help="Input vocal1 file")
    parser.add_argument("--target", required=True, help="Input vocal2 file")
    parser.add_argument("--output", required=True, help="Output shadow wav")
    parser.add_argument("--sample-rate", type=int, default=DEFAULT_SR)
    parser.add_argument("--mix", type=float, default=0.85,
                        help="Overall shadow gain after contour matching")
    parser.add_argument("--max-shift-semitones", type=float, default=DEFAULT_MAX_SHIFT_SEMITONES,
                        help="Maximum allowed pitch bend in either direction")
    parser.add_argument("--smoothing-kernel", type=int, default=DEFAULT_SMOOTHING_KERNEL,
                        help="Odd median-filter kernel for ratio smoothing")
    parser.add_argument("--max-lag-ms", type=float, default=DEFAULT_MAX_LAG_MS,
                        help="Maximum lag compensation window when aligning takes")
    return parser.parse_args()


def decode_audio(path: Path, sample_rate: int, workdir: Path) -> np.ndarray:
    decoded = workdir / f"{path.stem}.mono.wav"
    subprocess.run(
        [
            "ffmpeg", "-hide_banner", "-y",
            "-i", str(path),
            "-ac", "1",
            "-ar", str(sample_rate),
            "-c:a", "pcm_s16le",
            str(decoded),
        ],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    sr, data = wavfile.read(decoded)
    if sr != sample_rate:
        raise RuntimeError(f"decoded {path} at {sr}Hz, expected {sample_rate}Hz")
    if data.dtype == np.int16:
        audio = data.astype(np.float32) / 32768.0
    else:
        audio = data.astype(np.float32)
    return audio


def bandpass_voice(audio: np.ndarray, sr: int) -> np.ndarray:
    nyq = sr * 0.5
    b, a = butter(2, [90.0 / nyq, 3500.0 / nyq], btype="bandpass")
    return filtfilt(b, a, audio).astype(np.float32)


def frame_count(n_samples: int) -> int:
    if n_samples <= FRAME_LENGTH:
        return 1
    return 1 + math.ceil((n_samples - FRAME_LENGTH) / HOP_LENGTH)


def extract_frame(audio: np.ndarray, index: int) -> np.ndarray:
    start = index * HOP_LENGTH
    frame = np.zeros(FRAME_LENGTH, dtype=np.float32)
    end = min(len(audio), start + FRAME_LENGTH)
    frame[:max(0, end - start)] = audio[start:end]
    return frame


def normalize_track(track: np.ndarray) -> np.ndarray:
    if not np.any(track > 0):
        return np.zeros_like(track, dtype=np.float32)
    scale = float(np.percentile(track[track > 0], 95))
    if scale <= 1e-6:
        return np.zeros_like(track, dtype=np.float32)
    return np.clip(track / scale, 0.0, 1.0).astype(np.float32)


def detect_pitch_track(audio: np.ndarray, sr: int) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    filtered = bandpass_voice(audio, sr)
    n = frame_count(len(filtered))
    pitches = np.zeros(n, dtype=np.float32)
    voiced = np.zeros(n, dtype=np.float32)
    activity = np.zeros(n, dtype=np.float32)
    min_lag = int(sr / MAX_F0)
    max_lag = int(sr / MIN_F0)

    for i in range(n):
        frame = extract_frame(filtered, i)
        rms = float(np.sqrt(np.mean(frame * frame)))
        activity[i] = rms
        if rms < MIN_RMS:
            continue
        frame = frame - frame.mean()
        windowed = frame * np.hanning(len(frame)).astype(np.float32)
        acf = correlate(windowed, windowed, mode="full")[len(windowed) - 1:]
        zero = float(acf[0]) if acf.size else 0.0
        if zero <= 1e-8:
            continue
        search = acf[min_lag:max_lag]
        if search.size == 0:
            continue
        rel = int(np.argmax(search))
        peak = float(search[rel])
        corr = peak / zero
        if corr < MIN_VOICED_CORR:
            continue
        lag = min_lag + rel
        freq = sr / lag
        pitches[i] = freq
        voiced[i] = min(1.0, max(0.0, (corr - MIN_VOICED_CORR) / (1.0 - MIN_VOICED_CORR)))

    valid = pitches > 0
    if np.any(valid):
        smoothed = medfilt(pitches, kernel_size=7)
        pitches[valid] = smoothed[valid]
    activity = normalize_track(activity)
    if len(activity) >= 7:
        activity = medfilt(activity, kernel_size=7).astype(np.float32)
    return pitches, voiced, activity


def fill_pitch_gaps(pitches: np.ndarray) -> np.ndarray:
    out = pitches.astype(np.float32).copy()
    idx = np.nonzero(out > 0)[0]
    if idx.size == 0:
        return out
    full = np.arange(len(out))
    interp = np.interp(full, idx, out[idx]).astype(np.float32)
    out[out == 0] = interp[out == 0]
    return out


def shift_track(track: np.ndarray, shift: int) -> np.ndarray:
    out = np.zeros_like(track, dtype=np.float32)
    if shift == 0:
        return track.astype(np.float32)
    if shift > 0:
        if shift < len(track):
            out[shift:] = track[:-shift]
        return out
    shift = abs(shift)
    if shift < len(track):
        out[:-shift] = track[shift:]
    return out


def estimate_lag_frames(source_activity: np.ndarray, target_activity: np.ndarray,
                        max_lag_frames: int) -> int:
    src = source_activity.astype(np.float32) - float(np.mean(source_activity))
    tgt = target_activity.astype(np.float32) - float(np.mean(target_activity))
    corr = correlate(tgt, src, mode="full")
    lags = correlation_lags(len(tgt), len(src), mode="full")
    mask = (lags >= -max_lag_frames) & (lags <= max_lag_frames)
    if not np.any(mask):
        return 0
    return int(lags[mask][int(np.argmax(corr[mask]))])


def cumulative_profile(activity: np.ndarray) -> np.ndarray:
    mass = np.maximum(activity.astype(np.float32), 0.0) + 1e-4
    csum = np.cumsum(mass)
    return (csum / max(float(csum[-1]), 1e-6)).astype(np.float32)


def build_target_to_source_map(source_activity: np.ndarray, target_activity: np.ndarray,
                               lag_frames: int) -> np.ndarray:
    aligned_source = shift_track(source_activity, lag_frames)
    src_profile = cumulative_profile(aligned_source)
    tgt_profile = cumulative_profile(target_activity)
    aligned_indices = np.interp(
        tgt_profile,
        src_profile,
        np.arange(len(source_activity), dtype=np.float32),
    ).astype(np.float32)
    original_indices = aligned_indices - float(lag_frames)
    return np.clip(original_indices, 0.0, float(len(source_activity) - 1))


def sample_track(track: np.ndarray, frame_positions: np.ndarray) -> np.ndarray:
    return np.interp(
        frame_positions,
        np.arange(len(track), dtype=np.float32),
        track.astype(np.float32),
    ).astype(np.float32)


def ratio_track(source_pitch: np.ndarray, target_pitch: np.ndarray,
                source_voiced: np.ndarray, target_voiced: np.ndarray,
                source_activity: np.ndarray, target_activity: np.ndarray,
                source_map: np.ndarray,
                max_shift_semitones: float, smoothing_kernel: int) -> tuple[np.ndarray, np.ndarray]:
    src_filled = fill_pitch_gaps(source_pitch)
    tgt_filled = fill_pitch_gaps(target_pitch)
    src = sample_track(src_filled, source_map)
    src_vox = sample_track(source_voiced, source_map)
    src_act = sample_track(source_activity, source_map)
    tgt = tgt_filled.astype(np.float32)
    ratio = np.ones_like(tgt, dtype=np.float32)
    active = (src_vox > 0.05) & (target_voiced > 0.05) & (src > 0) & (tgt > 0)
    raw = np.ones_like(tgt, dtype=np.float32)
    raw[active] = tgt[active] / np.maximum(src[active], 1e-6)
    max_ratio = float(2.0 ** (max_shift_semitones / 12.0))
    min_ratio = 1.0 / max_ratio
    raw = np.clip(raw, min_ratio, max_ratio)
    kernel = max(1, int(smoothing_kernel))
    if kernel % 2 == 0:
        kernel += 1
    if np.any(active) and kernel > 1:
        ratio = medfilt(raw, kernel_size=kernel).astype(np.float32)
    else:
        ratio = raw
    env = np.sqrt(np.minimum(src_act, target_activity)).astype(np.float32)
    env *= np.minimum(src_vox + 0.15, 1.0)
    env *= np.minimum(target_voiced + 0.15, 1.0)
    env = np.clip(env, 0.0, 1.0).astype(np.float32)
    return ratio, env


def granular_pitch_shift(audio: np.ndarray, source_map: np.ndarray,
                         ratios: np.ndarray, env: np.ndarray) -> np.ndarray:
    out = np.zeros(len(audio) + FRAME_LENGTH * 2, dtype=np.float32)
    wsum = np.zeros(len(out), dtype=np.float32)
    win = np.hanning(FRAME_LENGTH).astype(np.float32)
    n = len(ratios)
    base = np.arange(FRAME_LENGTH, dtype=np.float32) - (FRAME_LENGTH / 2.0)
    src_index = np.arange(len(audio), dtype=np.float32)

    for i in range(n):
        center = i * HOP_LENGTH + FRAME_LENGTH // 2
        target_start = i * HOP_LENGTH
        source_center = source_map[i] * HOP_LENGTH + FRAME_LENGTH / 2.0
        sample_positions = source_center + base
        chunk = np.interp(sample_positions, src_index, audio, left=0.0, right=0.0).astype(np.float32)
        chunk *= win
        ratio = max(0.5, min(2.0, float(ratios[i])))
        new_len = max(128, int(round(FRAME_LENGTH / ratio)))
        shifted = resample(chunk, new_len).astype(np.float32)
        grain = np.zeros(FRAME_LENGTH, dtype=np.float32)
        if new_len >= FRAME_LENGTH:
            src_off = (new_len - FRAME_LENGTH) // 2
            grain[:] = shifted[src_off:src_off + FRAME_LENGTH]
        else:
            off = (FRAME_LENGTH - new_len) // 2
            grain[off:off + new_len] = shifted
        gain = float(env[i])
        grain *= gain
        dst0 = target_start + FRAME_LENGTH
        dst1 = dst0 + FRAME_LENGTH
        out[dst0:dst1] += grain * win
        wsum[dst0:dst1] += (win * win) * gain

    out = out[FRAME_LENGTH:FRAME_LENGTH + len(audio)]
    wsum = wsum[FRAME_LENGTH:FRAME_LENGTH + len(audio)]
    return out / np.maximum(wsum, 1e-4)


def soft_normalize(audio: np.ndarray, target_peak: float = 0.92) -> np.ndarray:
    peak = float(np.max(np.abs(audio))) if audio.size else 0.0
    if peak <= 1e-6:
        return audio
    return (audio * min(target_peak / peak, 1.0)).astype(np.float32)


def main() -> None:
    args = parse_args()
    source = Path(args.source).expanduser().resolve()
    target = Path(args.target).expanduser().resolve()
    output = Path(args.output).expanduser().resolve()

    with tempfile.TemporaryDirectory() as td:
        workdir = Path(td)
        src_audio = decode_audio(source, args.sample_rate, workdir)
        tgt_audio = decode_audio(target, args.sample_rate, workdir)

        max_len = max(len(src_audio), len(tgt_audio))
        src_audio = np.pad(src_audio, (0, max_len - len(src_audio)))
        tgt_audio = np.pad(tgt_audio, (0, max_len - len(tgt_audio)))

        src_pitch, src_voiced, src_activity = detect_pitch_track(src_audio, args.sample_rate)
        tgt_pitch, tgt_voiced, tgt_activity = detect_pitch_track(tgt_audio, args.sample_rate)
        max_lag_frames = max(1, int(round((args.max_lag_ms / 1000.0) * args.sample_rate / HOP_LENGTH)))
        lag_frames = estimate_lag_frames(src_activity, tgt_activity, max_lag_frames)
        source_map = build_target_to_source_map(src_activity, tgt_activity, lag_frames)
        ratios, env = ratio_track(
            src_pitch,
            tgt_pitch,
            src_voiced,
            tgt_voiced,
            src_activity,
            tgt_activity,
            source_map,
            args.max_shift_semitones,
            args.smoothing_kernel,
        )

        shadow = granular_pitch_shift(src_audio, source_map, ratios, env)
        shadow = bandpass_voice(shadow, args.sample_rate) * float(args.mix)
        shadow = soft_normalize(shadow)

        output.parent.mkdir(parents=True, exist_ok=True)
        wavfile.write(output, args.sample_rate, np.int16(np.clip(shadow, -1.0, 1.0) * 32767.0))


if __name__ == "__main__":
    main()
