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

## Basic mix (vocal + banjo + bass + vocal2)

`bounce_vocal_banjo_bass_vocal2.sh` mirrors the banjo/bass/harp flow, but
treats the vocal2 overdub like the accordion lane in other mixes.

Usage:

```sh
./bounce_vocal_banjo_bass_vocal2.sh t1.wav t2.wav t3.wav t4.wav out.wav
```

## Basic mix (vocal + bass + harp + harp2)

`bounce_vocal_bass_harp_harp2.sh` balances lead vocal with bass plus two
harp overdubs (harp1 higher/trumpet-ish, harp2 lower/sax-ish) with mild
stereo width.

Usage:

```sh
./bounce_vocal_bass_harp_harp2.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```

## Basic mix (spoken word vocal + banjo + bass + accordion)

`bounce_vocal_banjo_bass_accordion.sh` balances a spoken word vocal with
banjo slightly left and accordion slightly right.

Usage:

```sh
./bounce_vocal_banjo_bass_accordion.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```

## Mix two bounces (background at 30%)

`bounce_mix_ducked_background.sh` blends two existing bounces, keeping the
second input at 30% volume.

Usage:

```sh
./bounce_mix_ducked_background.sh fg.wav bg.wav out.wav [out.mp3]
```

## Basic mix (vocal + vocal2 + viola + accordion)

`bounce_vocal_vocal2_viola_accordion.sh` balances a lead vocal with a
harmony vocal plus viola left and accordion right.

Usage:

```sh
./bounce_vocal_vocal2_viola_accordion.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```

## Basic mix (vocal + vocal2 + bass + accordion)

`bounce_vocal_vocal2_bass_accordion.sh` balances a lead vocal with a
harmony vocal plus bass and accordion.

Usage:

```sh
./bounce_vocal_vocal2_bass_accordion.sh t1.wav t2.wav t3.wav t4.wav out.wav [out.mp3]
```

## Vocal-forward mix (dual vocals + banjo + bass + accordion)

`bounce_vocals_banjo_bass_accordion_vocal_forward.sh` extends the banjo/bass
flow for two vocal tracks panned left/right plus accordion.

Usage:

```sh
./bounce_vocals_banjo_bass_accordion_vocal_forward.sh t1.wav t2.wav t3.wav t4.wav t5.wav out.wav [out.mp3]
```
