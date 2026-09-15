-- | **Laws for the revoicing transforms.**
-- |
-- | Revoicing is a handful of small edits — invert, octave-shift, drop a note,
-- | place one, double one — and each is easy to write and easy to get subtly
-- | wrong, because they interact: an inversion changes which note is lowest,
-- | which changes what an octave shift means, which changes which stack
-- | position each note is read against, which changes which tones look absent.
-- | The bugs that result are not crashes. They are a chord that sounds nearly
-- | right, or a ghost note offered for a note that is already playing.
-- |
-- | So these are LAWS rather than examples: properties that must hold for every
-- | chord from every source, checked across the whole Banks grid, the McMullen
-- | palette, the Butler bank and the diatonic triads. An example test proves a
-- | transform works on a C major triad; a law catches the sixth-chord in the
-- | far bank that the example never reached.
module Test.RevoiceSpec (runRevoiceTests) where

import Prelude

import Data.Array (all, concatMap, filter, head, last, length, nub, sort, (..))
import Data.Array as Array
import Data.Foldable (elem, foldl, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord (Key, Mode(..))
import Harmonia.OpenVoicing (at, doubleTone, dropAt, octaves, setTone, toggleTone) as OV
import Test.Assert (assertTrue')
import Vetula.Banks (butlerChords)
import Vetula.Harmony (ChordNode, bassMidi, diatonicTriads, mcmullenChords, octaveShift, playNotes)
import Vetula.Pads as Pads
import Vetula.Spread (applyToNode, ghostRows, invertNode, nextBassTone, openFor, refootNode, spreadOfNode, toneRows)

cMajor :: Key
cMajor = { tonic: 0, mode: Ionian }

-- | Every chord the app can put under the ladder, from every source that makes
-- | one. The point of the suite is that a law holds for all of them.
corpus :: Array ChordNode
corpus =
  diatonicTriads cMajor
    <> mcmullenChords cMajor
    <> butlerChords cMajor
    <> concatMap _.chords (Pads.grid cMajor 1)

pcsOf :: Array Int -> Array Int
pcsOf = sort <<< nub <<< map (\m -> mod m 12)

-- | How many notes you actually HEAR: distinct sounding pitches, so two voices
-- | on one pitch count once.
heardCount :: ChordNode -> Int
heardCount = length <<< nub <<< playNotes

-- | Check a law over the whole corpus, naming the first chord that breaks it.
law :: String -> (ChordNode -> Boolean) -> Effect Unit
law name holds = do
  let bad = filter (not <<< holds) corpus
  for_ (head bad) \c ->
    log ("      first failure: " <> c.label <> "  " <> show (playNotes c))
  assertTrue' (name <> " (" <> show (length bad) <> " of " <> show (length corpus) <> " failed)")
    (length bad == 0)

runRevoiceTests :: Effect Unit
runRevoiceTests = do
  log ("\n--- Vetula revoicing — transform laws over " <> show (length corpus) <> " chords ---")

  -- ── Octave shift ───────────────────────────────────────────────────────
  -- The transform that MUST move every note, or it is spreading the chord
  -- rather than transposing it.
  law "8ve up moves every note up exactly 12"
    (\c -> playNotes (octaveShift 1 c) == map (_ + 12) (playNotes c))
  law "8ve down moves every note down exactly 12"
    (\c -> playNotes (octaveShift (-1) c) == map (_ - 12) (playNotes c))
  law "8ve up then down is the identity"
    (\c -> playNotes (octaveShift (-1) (octaveShift 1 c)) == playNotes c)
  law "8ve shift preserves pitch-class content"
    (\c -> pcsOf (playNotes (octaveShift 1 c)) == pcsOf (playNotes c))
  -- The symptom AC reported: a chord parked where the control does nothing.
  law "8ve up actually moves the chord"
    (\c -> playNotes (octaveShift 1 c) /= playNotes c)
  law "8ve down actually moves the chord"
    (\c -> playNotes (octaveShift (-1) c) /= playNotes c)

  -- ── Inversion ──────────────────────────────────────────────────────────
  law "invert preserves pitch-class content"
    (\c -> pcsOf (playNotes (invertNode 1 c)) == pcsOf (playNotes c))
  law "invert preserves the note count"
    (\c -> length (playNotes (invertNode 1 c)) == length (playNotes c))
  -- NOT an exact inverse on a widely-spaced chord: "lowest up" and "highest
  -- down" only undo each other when the note that moved up lands on top. What
  -- must hold in both directions is that the chord's content survives.
  law "invert up then down preserves pitch-class content"
    (\c -> pcsOf (playNotes (invertNode (-1) (invertNode 1 c))) == pcsOf (playNotes c))
  law "invert down preserves pitch-class content"
    (\c -> pcsOf (playNotes (invertNode (-1) c)) == pcsOf (playNotes c))
  -- Guarded on having more than one tone to re-foot ON: Butler's `oct` is a
  -- bank of pure octaves, and no inversion of a single pitch class exists.
  law "repeated inversion eventually re-foots the chord on a new tone"
    (\c -> let steps = Array.scanl (\d _ -> invertNode 1 d) c (1 .. 6)
           in length (pcsOf (playNotes c)) < 2 || length (nub (map _.bassPc steps)) > 1)
  law "the bass is the lowest note after inverting"
    (\c -> let d = invertNode 1 c in head (sort (playNotes d)) == Just (bassMidi d))
  law "the bass is the lowest note as built"
    (\c -> head (sort (playNotes c)) == Just (bassMidi c))
  -- The register must not drift away under the uppers: a full turn of the
  -- inversions should raise the WHOLE chord, not strand the bass below it.
  law "inverting keeps the bass within two octaves of the lowest upper"
    (\c -> let d = invertNode 1 (invertNode 1 (invertNode 1 c))
               lo = fromMaybe (bassMidi d) (head (sort d.voicing))
           in lo - bassMidi d <= 24)
  -- And the two controls must remain independent of each other.
  law "8ve up still moves the chord after three inversions"
    (\c -> let d = invertNode 1 (invertNode 1 (invertNode 1 c))
           in playNotes (octaveShift 1 d) == map (_ + 12) (playNotes d))

  -- ── Ghosts ─────────────────────────────────────────────────────────────
  -- A ghost claims "this tone is not sounding". It must never claim that of a
  -- tone you can hear.
  law "no ghost for a pitch class that is sounding"
    (\c -> let heard = pcsOf (playNotes c)
           in all (\r -> not (elem r.pc heard)) (ghostRows c))
  law "a complete chord offers no ghosts at all"
    (\c -> length (ghostRows c) == 0)
  law "a complete chord offers no ghosts after inverting"
    (\c -> length (ghostRows (invertNode 1 c)) == 0)
  law "a complete chord offers no ghosts after an octave shift"
    (\c -> length (ghostRows (octaveShift 1 c)) == 0)
  law "ghost rows are distinct by pitch class"
    (\c -> let ps = map _.pc (ghostRows c) in length (nub ps) == length ps)

  -- ── Reading a voicing back ─────────────────────────────────────────────
  law "a voicing round-trips through its spread"
    (\c -> playNotes (applyToNode c (spreadOfNode c)) == playNotes c)
  law "a voicing round-trips after inverting"
    (\c -> let d = invertNode 1 c in playNotes (applyToNode d (spreadOfNode d)) == playNotes d)

  -- ── Dropping and restoring ─────────────────────────────────────────────
  -- Dropping one copy of a tone must leave the others alone: this is the bug
  -- where option-dragging a note and then shift-clicking it silenced both.
  law "dropping one copy of a doubled tone leaves the other sounding"
    (\c ->
      let sp = spreadOfNode c
          doubled = applyToNode c (OV.doubleTone (openFor c) 0 sp)
          rows = toneRows doubled
          ks = case Array.head rows of
            Just r -> OV.octaves r.place
            Nothing -> []
      in case last (sort ks) of
           Nothing -> true
           Just k -> length (playNotes (applyToNode doubled (OV.dropAt 0 k (spreadOfNode doubled))))
                       == length (playNotes doubled) - 1)
  law "omitting a tone removes at least one note"
    (\c -> case Array.head (toneRows c) of
        Nothing -> true
        Just _ -> length (playNotes (applyToNode c (OV.toggleTone 0 (spreadOfNode c))))
                    < length (playNotes c))
  law "an omitted tone becomes a ghost"
    (\c -> case Array.head (toneRows c) of
        Nothing -> true
        Just r ->
          let d = applyToNode c (OV.toggleTone 0 (spreadOfNode c))
          in if elem r.pc (pcsOf (playNotes d)) then true
             else elem r.pc (map _.pc (ghostRows d)))
  law "the bass survives every edit"
    (\c -> let d = applyToNode c (OV.toggleTone 0 (spreadOfNode c))
           in head (playNotes d) == head (playNotes c))

  -- ── Note count ─────────────────────────────────────────────────────────
  -- AC: the transforms that REPOSITION a chord must all be note-count
  -- preserving, and the ones that add or remove a note must do so by an exact
  -- amount. Stated together, because "the chord quietly gained or lost a note"
  -- is the failure they share and the one you notice last — it does not look
  -- wrong on the ladder, it just sounds thinner.
  law "invert up preserves the note count"
    (\c -> length (playNotes (invertNode 1 c)) == length (playNotes c))
  law "invert down preserves the note count"
    (\c -> length (playNotes (invertNode (-1) c)) == length (playNotes c))
  law "8ve shift preserves the note count"
    (\c -> length (playNotes (octaveShift 1 c)) == length (playNotes c))
  law "reading a voicing back preserves the note count"
    (\c -> length (playNotes (applyToNode c (spreadOfNode c))) == length (playNotes c))

  -- ── Heard notes, not array entries (2026-09-15) ────────────────────────
  -- The counts above are blind to a UNISON: `invert` moves one entry and can
  -- land it on a pitch the chord is already sounding, so the array stays four
  -- long and you hear three. Harmonia measured it at 108 of 960 voicings on a
  -- full rotation before `freeOctave` fixed it; these are the same law on the
  -- chords actually in the app. Deliberate doubling still passes — the test is
  -- that a transform may not INVENT a unison, not that unisons are illegal.
  law "invert up invents no unison"
    (\c -> heardCount (invertNode 1 c) == heardCount c)
  law "invert down invents no unison"
    (\c -> heardCount (invertNode (-1) c) == heardCount c)
  law "a full rotation invents no unison"
    (\c -> let n = length (playNotes c)
               rotated = foldl (\d _ -> invertNode 1 d) c (1 .. n)
           in heardCount rotated == heardCount c)
  law "an octave shift invents no unison"
    (\c -> heardCount (octaveShift 1 c) == heardCount c)
  law "re-footing onto every chord tone invents no unison"
    (\c -> all (\pc -> heardCount (refootNode pc c) == heardCount c) (pcsOf (playNotes c)))

  -- ── Bass substitution ──────────────────────────────────────────────────
  -- Untested until now, because it lived inline in the component where nothing
  -- could reach it.
  law "re-footing preserves the note count"
    (\c -> length (playNotes (refootNode (nextBassTone 1 c) c)) == length (playNotes c))
  law "re-footing preserves pitch-class content"
    (\c -> pcsOf (playNotes (refootNode (nextBassTone 1 c) c)) == pcsOf (playNotes c))
  law "re-footing puts the named tone in the bass"
    (\c -> let pc = nextBassTone 1 c in (refootNode pc c).bassPc == pc)
  law "re-footing onto the current bass changes nothing"
    (\c -> playNotes (refootNode c.bassPc c) == playNotes c)
  law "re-footing onto every chord tone in turn preserves the count"
    (\c -> all (\pc -> length (playNotes (refootNode pc c)) == length (playNotes c))
             (pcsOf (playNotes c)))
  law "re-footing onto every chord tone in turn preserves the content"
    (\c -> all (\pc -> pcsOf (playNotes (refootNode pc c)) == pcsOf (playNotes c))
             (pcsOf (playNotes c)))
  law "the bass is still the lowest note after re-footing"
    (\c -> let d = refootNode (nextBassTone 1 c) c
           in head (sort (playNotes d)) == Just (bassMidi d))

  -- ── Adding and removing, by an exact amount ────────────────────────────
  law "doubling a tone adds exactly one note"
    (\c -> length (playNotes (applyToNode c (OV.doubleTone (openFor c) 0 (spreadOfNode c))))
             == length (playNotes c) + 1)
  law "omitting a tone removes exactly the copies it had"
    (\c -> case Array.head (toneRows c) of
        Nothing -> true
        Just r ->
          length (playNotes (applyToNode c (OV.toggleTone 0 (spreadOfNode c))))
            == length (playNotes c) - length (OV.octaves r.place))
  -- Guarded on a ghost actually being offered. Where omitting a tone leaves its
  -- pitch class still sounding — Butler's `oct` bank is pure octaves of one
  -- note — no ghost is drawn and the UI has no way to ask for the restore, so
  -- there is nothing here to assert.
  law "restoring an offered ghost adds exactly one note"
    (\c -> let d = applyToNode c (OV.toggleTone 0 (spreadOfNode c))
           in length (ghostRows d) == 0
              || length (playNotes (applyToNode d (OV.setTone 0 (OV.at 0) (spreadOfNode d))))
                   == length (playNotes d) + 1)
  law "dropping one copy removes exactly one note"
    (\c -> case Array.head (toneRows c) of
        Nothing -> true
        Just r -> case Array.head (sort (OV.octaves r.place)) of
          Nothing -> true
          Just k -> length (playNotes (applyToNode c (OV.dropAt 0 k (spreadOfNode c))))
                      == length (playNotes c) - 1)

  log "  ✓ revoicing laws"
