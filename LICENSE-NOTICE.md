# License Notice

Triggerfish is licensed under the **GNU General Public License, version 3 or
any later version (SPDX: `GPL-3.0-or-later`)**. The full text is in `LICENSE`.

## Why GPL?

Triggerfish compiles in **Littorina** (`afcondon/littorina`), Tidal's pattern
engine, which is GPL-3.0-or-later because TidalCycles (Alex McLean and
contributors) is. A program that compiles Littorina in and is distributed is
GPL as a whole, so Triggerfish is too.

Its other dependencies are permissively licensed and stay so: reef and
binnacle (MIT) deliberately never import Littorina — the hosts sample patterns
with Littorina and hand reef plain values — which is what keeps them outside
the GPL.

## Third-party material

- `public/img/verrill-architeuthis-1882.webp` — A. E. Verrill, 1882; public
  domain (see `public/img/CREDITS.md`).
- `public/samples/amen.wav` — the "Amen, Brother" drum break (The Winstons,
  1969), included as the conventional test loop for Stellatus's onset import.
