# Audio bounce scripts

## Vocal-forward mix (vocal + bass + pbass + accordion)

`bounce_vocal_bass_pbass_accordion_vocal_forward.sh` is a vocal-forward
variant of the original `bounce_vocal_bass_pbass_accordion.sh` flow.

Highlights:
- Keeps vocals forward with a light presence boost.
- Ducks the instrument bus using a sidechain compressor keyed off vocals.
- Uses an `asplit` to avoid reusing the vocal stream label in the filter graph.
- Runs the same two-pass loudness normalization for consistent output.

Usage:

```sh
./bounce_vocal_bass_pbass_accordion_vocal_forward.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```

## Vocal-forward mix (vocal + banjo + bass + harmonica)

`bounce_vocal_banjo_bass_harp_vocal_forward.sh` keeps vocals forward using the
same sidechain-ducking approach as the pbass/accordion variant.

Usage:

```sh
./bounce_vocal_banjo_bass_harp_vocal_forward.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```
