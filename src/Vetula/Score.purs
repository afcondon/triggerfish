-- | **The score: progressions as chords on a grand staff.**
-- |
-- | One system a progression, one bar a chord, the chord's name above it and
-- | the voices playing it as letter badges beside the title
-- | (docs/kb/plans/vetula-one-surface.md). What is drawn is the progression
-- | as written: what a voice does to it (arp, drop2, strum) is the river's
-- | to show.
-- |
-- | ## The axis
-- |
-- | As in Quadrat's stave (`Quadrat.Stave`), the two staves are one
-- | continuous scale of diatonic steps, `d = octave * 7 + letter`, middle C
-- | at 35. A note's place on it comes from its SPELLING, not its pitch
-- | class: B♯3 and C4 are one key on the piano and two places on the staff.
-- |
-- | ## Spelling
-- |
-- | Quadrat spells everything with sharps, because a sample knows no key.
-- | Vetula does: a scale tone takes its scale degree's letter (B♭ in F, A♯
-- | in B), and anything outside the scale follows the key's side, flats in
-- | a key whose own spelling has any. There is no key signature, so every
-- | accidental is written and nothing ever needs a natural.
module Vetula.Score
  ( Spelling
  , Badge
  , Staff
  , Handlers
  , spelling
  , spellingOf
  , spell
  , spellChord
  , spellName
  , rootOfName
  , system
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Harmonia.Chord (Mode, modeIntervals)
import Harmonia.ScaleFit (fitsFor, reading)
import Triggerfish.Glyph (GlyphIcon)
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event (Event, EventType(..))

-- | The key, as far as spelling needs it: the tonic's letter (0 = C .. 6 =
-- | B), the scale's pitch classes from the tonic up, and which side
-- | out-of-scale notes are spelled on.
type Spelling = { letter :: Int, scale :: Array Int, flats :: Boolean }

-- | A voice playing the row's progression: its letter, what it does to the
-- | chords (the line's `# …`, as written), and whether it is muted.
type Badge = { n :: Int, letter :: String, how :: String, muted :: Boolean }

-- | One system. `active`: the chord (by position) last heard, lit.
type Staff =
  { title :: String
  , note :: String
  , top :: Boolean
  , chords :: Array (Array Int)
  , names :: Array String
  , badges :: Array Badge
  , active :: Maybe Int
  , selected :: Maybe { from :: Int, to :: Int }
  , ownKey :: Boolean
  , dragging :: Maybe Int
  , saved :: Maybe { tonic :: Int, scale :: Array Int, mode :: String, saved :: Boolean }
  -- the rebus of its chords as a set: the same chords in another order
  -- wear the same picture
  , rebus :: Array GlyphIcon
  -- each bar's length in beats, if a rhythm was tapped in (empty: a bar
  -- each); and the bar a take in progress has reached
  , beats :: Array Int
  , tapping :: Maybe Int
  }

-- | What a click does: hear a chord (by position), revoice it (the open
-- | progression only), open the row's progression (any other), select a
-- | run of chords (shift-click) for the scales that fit it, or let it go.
type Handlers i =
  { hear :: Int -> i
  , revoice :: Maybe (Int -> i)
  , select :: Int -> i
  , unselect :: i
  , readIn :: i
  , adopt :: Maybe (Int -> Mode -> i)
  -- the open progression's edits: by position
  , duplicate :: Maybe (Int -> i)
  , remove :: Maybe (Int -> i)
  , dropAt :: Maybe (Int -> i)
  -- a chord dragged from the side panel: allowed over a bar, dropped into it
  , padOver :: Maybe (Event -> i)
  , padDrop :: Maybe (Int -> i)
  -- a progression dragged onto a voice's badge: that voice reads it; the
  -- row's title, dragged, is that progression
  , voiceOver :: Event -> i
  , voiceDrop :: Int -> Event -> i
  , titleDrag :: Maybe (Event -> i)
  -- tapping a rhythm in (the open progression only)
  , tap :: Maybe { start :: i, stop :: i, clear :: i }
  -- a saved progression, sent to Quadrat to sample
  , toQuadrat :: Maybe i
  -- the progression open when the page last closed, offered back
  , resume :: Maybe { name :: String, act :: i }
  -- back to the lattice to take more chords in; save this version
  , addChords :: Maybe i
  , save :: Maybe i
  }

-- | The natural pitch class of each letter.
naturals :: Array Int
naturals = [ 0, 2, 4, 5, 7, 9, 11 ]

letterOf :: Char -> Int
letterOf = case _ of
  'C' -> 0
  'D' -> 1
  'E' -> 2
  'F' -> 3
  'G' -> 4
  'A' -> 5
  'B' -> 6
  _ -> 0

natural :: Int -> Int
natural l = fromMaybe 0 (Array.index naturals (l `mod` 7))

-- | The signed distance from `from` up or down to `to`, as pitch classes,
-- | in -6 .. 5: how far a letter's natural is from the note it spells.
pcDiff :: Int -> Int -> Int
pcDiff to from =
  let d = ((to - from) `mod` 12 + 12) `mod` 12
  in if d > 6 then d - 12 else d

-- | The spelling of a key, from the tonic's NAME (it carries the letter:
-- | "E♭", "F♯") and the scale's pitch classes from the tonic. A scale of
-- | seven takes one letter a degree; any other (pentatonic, octatonic)
-- | spells from the tonic's side alone.
spelling :: String -> Array Int -> Spelling
spelling tonicName scale =
  let
    l0 = maybe' 0 letterOf (SCU.charAt 0 tonicName)
    flatTonic = String.contains (String.Pattern "\x266d") tonicName
    accidentals =
      if Array.length scale == 7 then Array.mapWithIndex (\i pc -> pcDiff pc (natural (l0 + i))) scale else []
  in
    { letter: l0
    , scale
    , flats: flatTonic || Array.any (_ < 0) accidentals
    }
  where
  maybe' d f = case _ of
    Just x -> f x
    Nothing -> d

-- | The spelling of a scale given by its root and pitch classes (root
-- | first): the tonic letter that spells it with the fewest accidentals,
-- | flats breaking a tie (E♭ dorian, not D♯).
spellingOf :: Int -> Array Int -> Spelling
spellingOf root scale =
  let
    cost l = if Array.length scale == 7
      then Array.foldl (+) 0 (Array.mapWithIndex (\i p -> absI (pcDiff p (natural (l + i)))) scale)
      else absI (pcDiff root (natural l))
    letters = Array.filter (\l -> absI (pcDiff root (natural l)) <= 1) (Array.range 0 6)
    flatOf l = pcDiff root (natural l) < 0
    ranked = Array.sortBy (\a b -> compare (cost a) (cost b) <> compare (not (flatOf a)) (not (flatOf b))) letters
    l0 = fromMaybe 0 (Array.head ranked)
    accs = if Array.length scale == 7 then Array.mapWithIndex (\i p -> pcDiff p (natural (l0 + i))) scale else [ pcDiff root (natural l0) ]
  in { letter: l0, scale, flats: Array.any (_ < 0) accs }
  where
  absI x = if x < 0 then negate x else x

-- | A MIDI note's place on the diatonic axis and its accidental (−2 .. 2),
-- | spelled from the key alone: a scale tone takes its degree's letter; a
-- | chromatic one, in a flat key, a flat; otherwise the common borrowings,
-- | ♭2 ♭3 ♯4 ♭6 ♭7 from the tonic.
spell :: Spelling -> Int -> { d :: Int, acc :: Int }
spell sp n = place n (keyLetter sp (n `mod` 12))

-- | The letter the key gives a pitch class.
keyLetter :: Spelling -> Int -> Int
keyLetter sp pc = case Array.elemIndex pc sp.scale of
  Just i | Array.length sp.scale == 7 -> (sp.letter + i) `mod` 7
  _ ->
    let tonic = fromMaybe (natural sp.letter) (Array.head sp.scale)
        fromTonic = ((pc - tonic) `mod` 12 + 12) `mod` 12
        flat = sp.flats || Array.elem fromTonic [ 1, 3, 8, 10 ]
    in if Array.elem pc naturals then fromMaybe 0 (Array.elemIndex pc naturals)
       else if flat then fromMaybe 0 (Array.elemIndex ((pc + 1) `mod` 12) naturals)
       else fromMaybe 0 (Array.elemIndex ((pc + 11) `mod` 12) naturals)

-- | A note written on a given letter: its accidental, and the octave the
-- | WRITTEN note sits in (B♯3 is 60 and written in octave 3).
place :: Int -> Int -> { d :: Int, acc :: Int }
place n letter =
  let acc = pcDiff (n `mod` 12) (natural letter)
      oct = Int.floor (Int.toNumber (n - acc) / 12.0)
  in { d: oct * 7 + letter, acc }

-- | **A chord, spelled as a chord.** A tone in the key's scale is spelled as
-- | the key spells it; a chromatic one takes the letter of its interval
-- | above the root (the root itself by the key): a seventh is
-- | a seventh (C E G B♭, never A♯), a third a third. Where an interval could
-- | be read two ways the chord decides: a sharp fifth beside a major third
-- | and no fifth (augmented), a diminished seventh beside a minor third and
-- | a flat fifth. A tone the reading would push past a double accidental
-- | falls back to the key's spelling. `root`: the chord's root, if known.
spellChord :: Spelling -> Maybe Int -> Array Int -> Array { d :: Int, acc :: Int }
spellChord sp mroot notes = case mroot of
  Nothing -> map (spell sp) notes
  Just r ->
    let
      rootLetter = keyLetter sp r
      ivs = map (\n -> ((n - r) `mod` 12 + 12) `mod` 12) notes
      has i = Array.elem i ivs
      steps i = case i of
        0 -> 0
        1 -> 1
        2 -> 1
        3 -> if has 4 then 1 else 2      -- a minor third, or ♯9 over a major third
        4 -> 2
        5 -> 3
        6 -> if has 7 then 3 else 4      -- ♭5, or ♯11 over a fifth
        7 -> 4
        8 -> if has 4 && not (has 7) then 4 else 5   -- ♯5 (augmented), or ♭6/♭13
        9 -> if has 3 && has 6 && not (has 10) then 6 else 5   -- °7, or 6/13
        _ -> 6
      -- a tone in the key's scale keeps the key's spelling; only a
      -- chromatic one is spelled from the chord (C7's B♭ in C)
      inScale n = Array.length sp.scale == 7 && Array.elem (n `mod` 12) sp.scale
      one n =
        let i = ((n - r) `mod` 12 + 12) `mod` 12
            w = place n ((rootLetter + steps i) `mod` 7)
        in if inScale n || w.acc > 2 || w.acc < (-2) then spell sp n else w
    in map one notes

-- | The root a chord name begins with ("A♭m7", "C#/E", "Bbmaj7").
rootOfName :: String -> Maybe Int
rootOfName name = do
  c <- SCU.charAt 0 name
  base <- Array.index naturals (letterOf' c)
  let rest = SCU.drop 1 name
      acc = case SCU.charAt 0 rest of
        Just '#' -> 1
        Just '\x266f' -> 1
        Just 'b' -> -1
        Just '\x266d' -> -1
        _ -> 0
  pure (((base + acc) `mod` 12 + 12) `mod` 12)
  where
  letterOf' ch = if Array.elem ch [ 'C', 'D', 'E', 'F', 'G', 'A', 'B' ] then letterOf ch else 99

-- Geometry. A step is half a line-space.

step :: Number
step = 5.0

trebleLines :: Array Int
trebleLines = [ 37, 39, 41, 43, 45 ] -- E4 G4 B4 D5 F5 (d = floor (midi / 12) * 7 + letter)

bassLines :: Array Int
bassLines = [ 25, 27, 29, 31, 33 ] -- G2 B2 D3 F3 A3

topD :: Int
topD = 45

botD :: Int
botD = 25

clefRoom :: Number
clefRoom = 40.0

barWidth :: Number
barWidth = 74.0

-- | The extra room between the staves.
staffGap :: Number
staffGap = 14.0

-- | **A progression as one system**, with its header: the title (a button
-- | when the row can be opened), the badges of the voices playing it, and a
-- | note. Every bar shares the row's vertical extent, so a note that looks
-- | higher than its neighbour is.
system :: forall w i. Spelling -> Handlers i -> Staff -> HH.HTML w i
system pageSp on row =
  HH.div
    [ HP.style ("margin: 0 0 18px; padding: 10px 14px 6px; border-radius: 6px; user-select: none; -webkit-user-select: none; "
        <> (if row.top then "background: #fffdf6; border: 1px solid #d8cfb6;" else "background: #fbf8f0; border: 1px solid #ece5d0;")) ]
    ( [ header
      , HH.div [ HP.style "overflow-x: auto;" ] [ staff ]
      ] <> scales )
  where
  -- **What the progression is in.** The key it is shown against is the
  -- page's; its reading (Harmonia.ScaleFit) is the key-capable scale that
  -- leaves out fewest of its notes, from its own chords' roots. Spelled and
  -- coloured in the reading when the row asks (`ownKey`).
  allNotes = Array.concat row.chords
  roots = Array.mapMaybe rootOfName row.names
  read = reading roots allNotes
  -- a row is measured against the key its progression was saved in, when it
  -- has one; the open progression and written-in voices, the page's key
  keySp = case row.saved of
    Just k -> spellingOf k.tonic k.scale
    Nothing -> pageSp
  savedLine = case row.saved of
    Just k -> [ HH.span [ HP.style "font-size: 11px; color: #6c8792;" ]
                  [ HH.text (if k.saved then "saved in " else "in "), HH.span [ HP.style "font-weight: 600; color: #2f3e44;" ] [ HH.text (pcNameIn keySp k.tonic <> " " <> k.mode) ] ] ]
    Nothing -> []
  readScale r = map (\iv -> (r.root + iv) `mod` 12) (modeIntervals r.key)
  sp = case read of
    Just r | row.ownKey -> spellingOf r.root (readScale r)
    _ -> keySp
  outsideOf scale = Array.length (Array.nub (Array.filter (\pc -> not (Array.elem pc scale)) (map (_ `mod` 12) allNotes)))
  inKey = outsideOf keySp.scale
  better = case read of
    Just r | Array.length r.outside < inKey -> Just r
    _ -> Nothing

  readingLine = case better of
    Nothing -> []
    Just r ->
      let rsp = spellingOf r.root (readScale r)
          nm = pcNameIn rsp r.root <> " " <> r.mode
          outs = Array.length r.outside
      in
        [ HH.span [ HP.style "font-size: 11px; color: #6c8792;" ]
            [ HH.text (show inKey <> " note" <> plural inKey <> " outside it \x00b7 reads as ")
            , HH.span [ HP.style "font-weight: 600; color: #2f3e44;" ] [ HH.text nm ]
            , HH.text (if outs == 0 then "" else " (" <> show outs <> " outside)")
            ]
        , chip (if row.ownKey then "\x2713 spelled in " <> nm else "spell in " <> nm) ("show this row's notes, and colour them, as " <> nm) on.readIn
        ] <> case on.adopt of
          Just act | Array.length r.outside < inKey ->
            [ chip ("make " <> nm <> " the key") ("the key becomes " <> nm <> ", with nothing transposed: the lattice follows, and saving the progression keeps it") (act r.root r.key) ]
          _ -> []
  plural k = if k == 1 then "" else "s"
  chip label tip act =
    HH.button
      [ HP.style "border: 1px solid #c9d6db; background: #f3f6f7; color: #2f3e44; border-radius: 4px; padding: 1px 8px; font-size: 11px; cursor: pointer;"
      , HP.title tip, HE.onClick \_ -> act ]
      [ HH.text label ]

  header =
    HH.div [ HP.style "display: flex; align-items: baseline; gap: 10px; flex-wrap: wrap; margin-bottom: 2px;" ]
      ( [ HH.span ([ HP.style "font-size: 13px; color: #2a2a2a; font-weight: 600;", HP.title "drag it onto a voice to have the voice play it" ] <> titleDrags) [ HH.text row.title ] ]
          <> (if Array.null row.rebus then [] else
                [ HH.span [ HP.style "display: inline-flex; gap: 3px; align-items: center; opacity: 0.85;"
                          , HP.title "its chords as a set: the same chords in another order wear the same picture" ]
                    (map (\g -> HH.i [ HP.attr (HH.AttrName "class") ("fa-solid fa-" <> g.icon), HP.style ("font-size: 12px; color: " <> g.color <> ";") ] []) row.rebus) ])
          <> (case on.tap, row.tapping of
                Just t, Just at ->
                  [ HH.span [ HP.style "font-size: 11px; color: #fff; background: #b3261e; border-radius: 4px; padding: 1px 8px;" ]
                      [ HH.text ("\x25cf tapping, bar " <> show (at + 1) <> " of " <> show (Array.length row.chords) <> " \x00b7 space: next chord \x00b7 Esc: stop") ]
                  , chip "stop" "abandon this take; the rhythm stays as it was" t.stop ]
                Just t, Nothing | not (Array.null row.chords) ->
                  [ chip (if Array.null row.beats then "\x25cf tap rhythm" else "\x25cf retake")
                      "tap the rhythm in: the first chord sounds, space moves to the next; the time between presses is each chord's length"
                      t.start ]
                    <> (if Array.null row.beats then [] else
                          [ chip "\x00d7 rhythm" "back to one chord a bar" t.clear ])
                _, _ -> [])
          <> (case on.save of
                Just act -> [ chip "save" "save this version (\x2318S); its voices then play it" act ]
                Nothing -> [])
          <> (case on.addChords of
                Just act | not (Array.null row.chords) -> [ chip "+ chords" "back to the lattice to take more chords in" act ]
                _ -> [])
          <> (case on.resume of
                Just r | Array.null row.chords ->
                  [ chip ("resume " <> r.name) "reopen the progression you had open when the page last closed" r.act ]
                _ -> [])
          <> (case on.toQuadrat of
                Just act -> [ chip "\x25f4 Quadrat" "send to Quadrat to sample: its chords, one a beat, and their rebus" act ]
                Nothing -> [])
          <> (case on.revoice of
                Just act | not (Array.null row.chords) ->
                  [ chip "revoice" "every chord's notes, side by side, the voices joined bar to bar" (act (fromMaybe 0 row.active)) ]
                _ -> [])
          <> map badge row.badges
          <> (if row.note == "" then [] else [ HH.span [ HP.style "font-size: 11px; color: #9a8d6a; font-style: italic;" ] [ HH.text row.note ] ])
          <> savedLine
          <> readingLine
      )

  titleDrags :: forall r. Array (HP.IProp r i)
  titleDrags = case on.titleDrag of
    Just act -> [ HP.attr (HH.AttrName "draggable") "true", HE.handler (EventType "dragstart") act ]
    Nothing -> []
  badge b =
    HH.span
      [ HP.style ("display: inline-flex; align-items: baseline; gap: 5px; padding: 1px 7px; border-radius: 9px; font-size: 11px; "
          <> (if b.muted then "background: #eee9db; color: #9a9070;" else "background: #5f6f6a; color: #fff;"))
      , HP.title ("voice " <> b.letter <> (if b.muted then ", muted" else "") <> " \x00b7 drop a progression here and this voice plays it")
      , HP.class_ (HH.ClassName "score-voice")
      , HE.handler (EventType "dragover") on.voiceOver
      , HE.handler (EventType "drop") (on.voiceDrop b.n) ]
      ( [ HH.span [ HP.style "font-weight: 700;" ] [ HH.text b.letter ] ]
          <> (if b.how == "" then [] else [ HH.span [ HP.style "opacity: 0.85;" ] [ HH.text b.how ] ])
          <> (if b.muted then [ HH.span [] [ HH.text "muted" ] ] else [])
      )

  -- each note spelled, and whether it lies outside the key's scale: red, as
  -- in the lattice's chord glyphs, so a chord's borrowed tones read at a glance
  spelt = Array.mapWithIndex
    (\i ns -> Array.zipWith (\m g -> { d: g.d, acc: g.acc, out: not (Array.null sp.scale) && not (Array.elem (m `mod` 12) sp.scale) })
                ns (spellChord sp (Array.index row.names i >>= rootOfName) ns))
    row.chords
  allDs = Array.concatMap (map _.d) spelt
  hiD = max (topD + 1) (fromMaybe topD (Array.foldr max' Nothing allDs))
  loD = min (botD - 1) (fromMaybe botD (Array.foldr min' Nothing allDs))
  max' x acc = Just (maybe x (max x) acc)
  min' x acc = Just (maybe x (min x) acc)
  maybe d f = case _ of
    Just x -> f x
    Nothing -> d

  labelH = 18.0
  revoiceH = if isJust on.revoice then 16.0 else 4.0
  pad = 6.0
  -- the staves sit apart, as engraved, rather than one line-space short
  -- of an eleven-line staff: everything below middle C moves down by the
  -- gap, so middle C stays with the treble (on its ledger) and B3 sits
  -- above the bass
  y d = labelH + pad + Int.toNumber (hiD - d) * step + (if d < 35 then staffGap else 0.0)
  staffBottom = y loD + pad
  height = staffBottom + revoiceH
  n = Array.length spelt
  width = clefRoom + Int.toNumber (max 1 n) * barWidth + 4.0
  colX i = clefRoom + Int.toNumber i * barWidth

  staff =
    el "svg"
      [ attr "viewBox" ("0 0 " <> show width <> " " <> show height)
      , attr "width" (show width)
      , attr "height" (show height)
      , attr "style" "display: block; user-select: none; -webkit-user-select: none;"
      ]
      -- the drag outline LAST: an element added before the bars shifts them,
      -- Halogen patches the nodes by position, and the press and release then
      -- land on different elements, so no click ever fires
      ( lit <> staffLines <> clefs <> barlines <> Array.concat (Array.mapWithIndex bar spelt) <> dragged <> chosen )

  lit = case row.active of
    Just i | i >= 0 && i < n ->
      [ el "rect" [ attr "x" (show (colX i)), attr "y" "0", attr "width" (show barWidth), attr "height" (show height)
                  , attr "fill" "#f2e7c6", attr "rx" "4" ] [] ]
    _ -> []
  -- a chord being dragged to a new place: drop it on another bar
  dragged = case row.dragging of
    Just i | i >= 0 && i < n ->
      [ el "rect" [ attr "x" (show (colX i)), attr "y" "0", attr "width" (show barWidth), attr "height" (show height)
                  , attr "fill" "none", attr "stroke" "#4f7a8c", attr "stroke-dasharray" "4 3", attr "rx" "4", attr "style" "pointer-events: none;" ] [] ]
    _ -> []
  -- the chords chosen for the scales: tinted, with a band beneath, drawn
  -- over everything (and after the bars, so nothing shifts under the mouse)
  chosen = case row.selected of
    Just sel ->
      let w = Int.toNumber (sel.to - sel.from + 1) * barWidth
      in [ el "rect" [ attr "x" (show (colX sel.from)), attr "y" "0", attr "width" (show w), attr "height" (show staffBottom)
                     , attr "fill" "#4f7a8c", attr "fill-opacity" "0.09", attr "stroke" "#4f7a8c", attr "stroke-opacity" "0.5", attr "rx" "4"
                     , attr "style" "pointer-events: none;" ] []
         , el "rect" [ attr "x" (show (colX sel.from + 2.0)), attr "y" (show (staffBottom - 3.0))
                     , attr "width" (show (w - 4.0)), attr "height" "3"
                     , attr "fill" "#4f7a8c", attr "rx" "1.5", attr "style" "pointer-events: none;" ] [] ]
    Nothing -> []

  -- **The scales that fit the chosen chords** (Harmonia.ScaleFit): every
  -- set that holds all their notes, named from their own roots first, then
  -- those one note short, saying which note. A list to take to the
  -- fretboard, not advice on what to play.
  scales = case row.selected of
    Nothing -> []
    Just sel ->
      let
        ixs = Array.range sel.from sel.to
        chosenNames = Array.mapMaybe (\i -> Array.index row.names i) ixs
        notes = Array.concat (Array.mapMaybe (\i -> Array.index row.chords i) ixs)
        -- every scale within three notes: when nothing holds them all, the
        -- closest still say what they leave out
        fits = fitsFor 3 (Array.mapMaybe rootOfName chosenNames) notes
        whole = Array.take 8 (Array.filter (\f -> Array.null f.outside) fits)
        near = Array.take (max 3 (6 - Array.length whole)) (Array.filter (\f -> not (Array.null f.outside)) fits)
        k = sel.to - sel.from + 1
        span = case Array.head chosenNames, Array.last chosenNames of
          Just a, Just b | k > 2 -> show k <> " chords: " <> spellName sp a <> " \x2026 " <> spellName sp b
          Just a, Just b | k == 2 -> "2 chords: " <> spellName sp a <> ", " <> spellName sp b
          Just a, _ -> spellName sp a
          _, _ -> ""
      in
        [ HH.div [ HP.style "margin: 4px 0 4px; padding: 8px 10px; background: #f3f6f7; border: 1px solid #d9e3e7; border-radius: 5px; font-size: 12px; color: #2f3e44;" ]
            ( [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 8px; margin-bottom: 6px;" ]
                  [ HH.span [ HP.style "font-size: 10px; letter-spacing: 0.1em; text-transform: uppercase; color: #6c8792;" ] [ HH.text "scales for" ]
                  , HH.span [ HP.style "font-weight: 600;" ] [ HH.text span ]
                  , HH.span [ HP.style "flex: 1;" ] []
                  , HH.button [ HP.style "border: none; background: none; color: #6c8792; cursor: pointer; font-size: 13px; padding: 0;"
                              , HP.title "put the scales away", HE.onClick \_ -> on.unselect ] [ HH.text "\x00d7" ]
                  ]
              ]
                <> (if Array.null whole then [ HH.div [ HP.style "color: #6c8792; font-style: italic; margin-bottom: 4px;" ] [ HH.text (if k > 1 then "no one scale holds every note of these chords; the closest:" else "no scale here holds every note; the closest:") ] ] else [])
                <> map fitRow whole
                <> map fitRow near
            )
        ]
  -- a scale's notes spelled within it, a letter a degree (A mixolydian ♭6
  -- has C♯, not D♭); its names likewise
  fitRow f =
    let fsp = case Array.head f.names of
          Just nm | Array.length f.pcs == 7 -> spellingOf nm.root (fromRoot f)
          _ -> sp
        spelt' = pcNameIn fsp
        names = map (\nm -> spelt' nm.root <> " " <> nm.mode) f.names
    in HH.div [ HP.style "display: flex; align-items: baseline; gap: 8px; padding: 2px 0;" ]
         ( [ HH.span [ HP.style "font-weight: 600; min-width: 13em;" ] [ HH.text (fromMaybe "" (Array.head names)) ]
           , HH.span [ HP.style "color: #6c8792;" ] [ HH.text (String.joinWith " \x00b7 " (map spelt' (fromRoot f))) ]
           ]
             <> (if Array.length names > 1 then [ HH.span [ HP.style "color: #8fa3ab; font-size: 11px;" ] [ HH.text ("also " <> String.joinWith ", " (Array.take 3 (Array.drop 1 names))) ] ] else [])
             <> (if Array.null f.outside then [] else [ HH.span [ HP.style "color: #c0392b; font-size: 11px;" ] [ HH.text ("leaves out " <> String.joinWith ", " (map spelt' f.outside)) ] ])
         )
  -- the scale's notes from the root it is named on
  fromRoot f = case Array.head f.names of
    Just nm -> Array.filter (_ >= nm.root) f.pcs <> Array.filter (_ < nm.root) f.pcs
    Nothing -> f.pcs
  -- a pitch class as the key spells it
  pcNameIn s r = let l = keyLetter s r in letterName l <> accGlyph (pcDiff r (natural l))

  hline x1 x2 d stroke =
    el "line" [ attr "x1" (show x1), attr "y1" (show (y d)), attr "x2" (show x2), attr "y2" (show (y d)), attr "stroke" stroke, attr "stroke-width" "1" ] []

  staffLines = map (\d -> hline 4.0 (width - 2.0) d "#8a8270") (trebleLines <> bassLines)

  clefs =
    [ clef "\x1d11e" 39 58.0
    , clef "\x1d122" 31 34.0
    ]
  clef glyph onLine size =
    el "text" [ attr "x" "6", attr "y" (show (y onLine)), attr "font-size" (show size), attr "dominant-baseline" "central", attr "fill" "#4a4232" ] [ HH.text glyph ]

  -- one bar a chord; the system closes with a double bar
  barlines =
    let top = y (topD)
        bot = y (botD)
        bl x w = el "line" [ attr "x1" (show x), attr "y1" (show top), attr "x2" (show x), attr "y2" (show bot), attr "stroke" "#8a8270", attr "stroke-width" w ] []
    in [ bl 4.0 "1" ] <> map (\i -> bl (colX (i + 1)) (if i == n - 1 then "2" else "1")) (Array.range 0 (n - 1))

  bar i notes =
    let
      x0 = colX i
      cx = x0 + barWidth / 2.0 + 6.0
      sorted = Array.sortWith _.d notes
      name = spellName sp (fromMaybe "" (Array.index row.names i))
      -- seconds are offset, as written: a head a step above the one below
      -- it moves right (and back for the next, so a cluster zig-zags)
      heads = (foldl (\acc g -> case acc.prev of
                  Just p | g.d - p.d == 1 && not acc.lastShifted ->
                    { prev: Just g, lastShifted: true, out: Array.snoc acc.out (Tuple g 7.0) }
                  _ -> { prev: Just g, lastShifted: false, out: Array.snoc acc.out (Tuple g 0.0) })
                { prev: Nothing, lastShifted: false, out: [] } sorted).out
      -- accidentals, top down, in two columns when they would collide
      accCols = (foldl (\acc g -> case acc.prevD of
                    Just pd | pd - g.d < 6 && acc.col == 0 -> { prevD: Just g.d, col: 1, out: Array.snoc acc.out (Tuple g 1) }
                    _ -> { prevD: Just g.d, col: 0, out: Array.snoc acc.out (Tuple g 0) })
                  { prevD: Nothing, col: 0, out: [] } (Array.reverse (Array.filter (\g -> g.acc /= 0) sorted))).out
      hits =
        el "rect"
          ( [ attr "x" (show x0), attr "y" "0", attr "width" (show barWidth), attr "height" (show staffBottom)
            , attr "fill" "transparent", attr "style" "cursor: pointer;"
            -- on the PRESS, not the click: the chord sounds at once, and the
            -- same press starts a drag (released on another bar, it moves)
            , HE.onMouseDown \e -> if ME.shiftKey e then on.select i else on.hear i ]
            <> (case on.dropAt of
                  Just act -> [ HE.onMouseUp \_ -> act i ]
                  Nothing -> [])
            <> (case on.padOver of
                  Just act -> [ HE.handler (EventType "dragover") act ]
                  Nothing -> [])
            <> (case on.padDrop of
                  Just act -> [ HE.handler (EventType "drop") \_ -> act i ]
                  Nothing -> [])
          ) [ el "title" [] [ HH.text ("hear " <> name <> " \x00b7 shift-click to choose chords for the scales that fit them") ] ]
      label =
        el "text" [ attr "x" (show (x0 + barWidth / 2.0)), attr "y" "13", attr "text-anchor" "middle", attr "font-size" "11", attr "fill" "#4a4232"
                  , attr "style" "pointer-events: none;" ] [ HH.text name ]
      -- the open progression's tools, under each bar: duplicate (to vary
      -- the copy), delete; revoicing is the whole row's, in its header
      tool dx glyph tip mact = case mact of
        Just act ->
          [ el "text"
              [ attr "x" (show (x0 + barWidth / 2.0 + dx)), attr "y" (show (staffBottom + 11.0)), attr "text-anchor" "middle", attr "font-size" "10"
              , attr "fill" "#8d7a4a", attr "style" "cursor: pointer;"
              , HE.onClick \_ -> act i ]
              [ el "title" [] [ HH.text tip ], HH.text glyph ] ]
        Nothing -> []
      revoiceBtn =
        tool (-6.0) "\x29c9" "duplicate this chord (then vary the copy)" on.duplicate
          <> tool 8.0 "\x00d7" "take this chord out" on.remove
    in
      [ hits, label ]
        -- its length, when the row has a rhythm
        <> (case Array.index row.beats i of
              Just b -> [ el "text" [ attr "x" (show (x0 + barWidth - 5.0)), attr "y" "13", attr "text-anchor" "end", attr "font-size" "9"
                                    , attr "fill" "#9a8d6a", attr "style" "pointer-events: none;" ]
                            [ el "title" [] [ HH.text (show b <> " beats") ], HH.text ("\x2669" <> show b) ] ]
              Nothing -> [])
        <> Array.concatMap (\g -> ledgersFor cx g.d) (Array.nubByEq (\a b -> a.d == b.d) sorted)
        <> map (\(Tuple g dx) -> el "ellipse" [ attr "cx" (show (cx + dx)), attr "cy" (show (y g.d)), attr "rx" "3.9", attr "ry" "2.9"
                                                 , attr "transform" ("rotate(-20 " <> show (cx + dx) <> " " <> show (y g.d) <> ")")
                                                 , attr "fill" (if g.out then "#c0392b" else "#1e1c17"), attr "style" "pointer-events: none;" ] []) heads
        <> map (\(Tuple g col) -> el "text" [ attr "x" (show (cx - 10.0 - Int.toNumber col * 8.0)), attr "y" (show (y g.d)), attr "dominant-baseline" "central"
                                             , attr "text-anchor" "middle", attr "font-size" "12", attr "fill" "#1e1c17", attr "style" "pointer-events: none;" ]
                                             [ HH.text (accGlyph g.acc) ]) accCols
        <> revoiceBtn

  -- ledger lines between a note and the staff it is outside, and middle C's
  ledgersFor cx d
    | d > topD = map (\k -> hline (cx - 7.0) (cx + 14.0) k "#8a8270") (Array.filter (\k -> (k - topD) `mod` 2 == 0) (Array.range (topD + 1) d))
    | d < botD = map (\k -> hline (cx - 7.0) (cx + 14.0) k "#8a8270") (Array.filter (\k -> (botD - k) `mod` 2 == 0) (Array.range d (botD - 1)))
    | d == 35 = [ hline (cx - 7.0) (cx + 7.0) 35 "#8a8270" ]
    | otherwise = []

-- | **A chord name as the notes spell it**: the root and any slash bass by
-- | the key (D♭, not C#), and the suffix's alterations as accidentals (m7♭5).
spellName :: Spelling -> String -> String
spellName sp nm = case String.split (String.Pattern "/") nm of
  [ chord, bass ] -> respellRoot (alterations chord) <> "/" <> respellRoot bass
  _ -> respellRoot (alterations nm)
  where
  respellRoot n = case rootOfName n of
    Nothing -> n
    Just r ->
      let l = keyLetter sp r
          acc = pcDiff r (natural l)
          rest = SCU.drop (if Array.elem (SCU.charAt 1 n) [ Just '#', Just 'b', Just '\x266f', Just '\x266d' ] then 2 else 1) n
      in letterName l <> accGlyph acc <> rest
  -- "b5" → "♭5", "#11" → "♯11", past the root (whose own b or # is its spelling)
  alterations n =
    let k = if Array.elem (SCU.charAt 1 n) [ Just '#', Just 'b' ] then 2 else 1
        head = SCU.take k n
        tail = SCU.drop k n
        swap from to t = String.replaceAll (String.Pattern from) (String.Replacement to) t
        fixed = foldl (\t d -> swap ("#" <> d) ("\x266f" <> d) (swap ("b" <> d) ("\x266d" <> d) t)) tail [ "13", "11", "9", "6", "5" ]
    in head <> fixed

letterName :: Int -> String
letterName l = fromMaybe "" (Array.index [ "C", "D", "E", "F", "G", "A", "B" ] l)

accGlyph :: Int -> String
accGlyph = case _ of
  1 -> "\x266f"
  2 -> "\x1d12a"
  (-1) -> "\x266d"
  (-2) -> "\x1d12b"
  _ -> ""

el :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
el name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)

attr :: forall r i. String -> String -> HH.IProp r i
attr k v = HP.attr (AttrName k) v
