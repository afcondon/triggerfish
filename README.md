# Triggerfish

Direct-manipulation instruments for live music: sequencers and polysignal
generators you play by dragging and turning rather than by typing.

Each instrument starts as a copy of a piece of hardware and then does what the
rack would make you patch cables and buy modules for:

- **Odonus** is a grid sequencer in the manner of René, with the Fugue Machine's
  move added: several playheads cross one shared grid, each at its own speed,
  direction and phase.
- **Vetula** builds chord progressions and voicings (pitch content from
  [harmonia](https://github.com/afcondon/purescript-harmonia)).
- **Balistes** is a Grids-style drum map whose accents come from internal
  Euclidean rhythms.
- **Conspicillum** is a grain machine whose scenes are written as one line.
- **Selene** makes many related signals from one source: LFOs, clocks,
  Euclids, envelopes, random sources and notes, laid across an ES-9 or an FH-2
  in a Eurorack case.
- **The Dashboard** is one page for the whole rig: which machines are open,
  play and stop for each, tempo and Link, and Panic.

Vetula and Odonus also run in the browser alone, with no rig, at
[trigger-fish.app](https://trigger-fish.app).

## How it fits together

Triggerfish is the face. The engines underneath are shared:

- [reef](https://github.com/afcondon/reef) holds the machine engines, written
  once in PureScript and compiled to both JavaScript (here) and Erlang.
- [Littorina](https://github.com/afcondon/littorina) is Tidal's pattern engine.
- [Architeuthis](https://github.com/afcondon/architeuthis) runs the same
  machines on the BEAM. With it running, Triggerfish keeps tempo over Ableton
  Link, plays MIDI to your synths and sends CV to the modular.
- [binnacle](https://github.com/afcondon/binnacle) gives the browser a
  Link-locked clock and a lookahead scheduler.

## Building

Needs PureScript 0.15.15, spago and Node 22 (the `.envrc` loads them through
Nix). `spago.yaml` takes several sibling repos by path, among them reef,
harmonia, Littorina, binnacle and the Hylograph libraries, so they must be
checked out at the relative paths it names. The private
`installing-triggerfish` repo sets all of that up on a new Mac.

```sh
make          # build every page's bundle into public/
spago test
```

Each page has its own bundle, so after changing shared code use `make`, not
`spago bundle`, or the other pages go stale. Serve `public/` with any static
server and open `index.html`.

## Licence

GPL-3.0-or-later, because Triggerfish compiles in Littorina, which is GPL as
Tidal is. See `LICENSE-NOTICE.md`.
