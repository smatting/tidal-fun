This is how I control Ableton Live on my Mac from tidal cycles: Everything is sent via MIDI.

## Usage

1. Follow tidalcycles instructions for installing SuperCollider and SuperDirt.
2. Evaluate `boot.sc` in Supercollider lin-by-line to start Superdirt and add the midi instrument
3. Use `ghci.sh` for the tidal interpreter
4. Use `BootTidal.hs` as the boot file`
5. Use `setupCCV` to set up the ccv->macro mappings for all 8 knobs for all tracks
6. Use Ableton Link feature to keep tidalcycles in sync..

Now you can send synchornized MIDI and macro modulation to all the tracks.

Optionally use adjust `dir-locals.el` for integration with `tidal=mode`.

```
BD -    _Bass Drum sounds
SD/SN - Snare Drum sounds
TH - Tom High
TM - Tom Mid
TL - Tom Low
OH - open hihat
HH/CH - (closed) hi hat
CL - clap / clave
RS - rim shot
MA - Maracas
CB - cow bell
CY - cymbal
PC - percussion
CO - Conga
```
