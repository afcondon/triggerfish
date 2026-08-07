-- | **The Euclidean clock** — the shared E(k,n) control, factored out of Selene's
-- | POLYEUCLID slots so Odonus's per-voice gates are the same instrument rather
-- | than a second idiom.
-- |
-- | The interaction, in one sentence: **click to select, arrows to edit** —
-- | ←/→ move `steps` (n), ↑/↓ move `beats` (k), shift takes a coarse stride.
-- | Nothing changes under the pointer without a selection first, which is what
-- | lets a ring be read at a glance and edited deliberately.
-- |
-- | Three separable pieces, so a caller takes only what it needs:
-- |
-- |   * `ring` — the pure figure (dots + optional playhead + `k/n`), styled.
-- |   * `cell` — the focusable, selectable box the figure sits in.
-- |   * `nudge` / `stepOf` — the arithmetic, once. `nudge` returns the new value
-- |     (a caller editing a record in hand); `stepOf` returns a signed axis+amount
-- |     (a caller that TRANSMITS relative edits, as Odonus's lockstep inputs do).
-- |     `stepOf` is defined against `nudge`, so the two can't drift apart.
-- |
-- | **On `hit`.** Selene's `euclidBits` and reef's `Reef.Odonus.euclidHit` are the
-- | same function — Bresenham, `(i * k) mod n < k`, with reef spelling out the
-- | `k <= 0` and `k >= n` ends that fall out of the arithmetic anyway. That is why
-- | one predicate here can serve both machines, and why this drawing is guaranteed
-- | to agree with what the reef engine actually plays. Keep it that way: if reef's
-- | rule ever changes, this one has to change with it.
-- |
-- | **On bounds.** The widget imposes no step ceiling of its own — `Bounds` is the
-- | caller's, because the honest limit is a property of the engine downstream, not
-- | of the picture. Odonus passes 16 because `Reef.Odonus` clamps `esteps` to 1..16
-- | inside `stepHead`, so a wider ring there would draw steps the engine never
-- | plays.
module Triggerfish.Ui.Euclid
  ( Euclid
  , Bounds
  , boundedBy
  , Style
  , defaultStyle
  , Chrome
  , defaultChrome
  , Select
  , Nudge(..)
  , Axis(..)
  , dirOf
  , axisOf
  , nudge
  , stepOf
  , hit
  , bits
  , ring
  , cell
  ) where

import Prelude

import Data.Array (range)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, pi)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- ---------------------------------------------------------------------------
-- The value
-- ---------------------------------------------------------------------------

-- | `beats` pulses spread evenly over `steps` steps — the k and n of E(k, n).
-- | Deliberately a bare record: Selene's `EuclidSlot` and reef's `Head` both
-- | carry these two numbers under their own field names, and each converts at
-- | the call site rather than this module learning about either.
type Euclid = { beats :: Int, steps :: Int }

-- | Editing limits. `coarse` is the shift-held stride.
type Bounds = { minSteps :: Int, maxSteps :: Int, coarse :: Int }

-- | The usual bounds for a given ceiling: down to a single step, shift striding
-- | by four (a bar's worth at sixteenths, so shift-← walks whole bars).
boundedBy :: Int -> Bounds
boundedBy maxSteps = { minSteps: 1, maxSteps, coarse: 4 }

-- ---------------------------------------------------------------------------
-- Arithmetic
-- ---------------------------------------------------------------------------

-- | An arrow. The convention across the rig: →/↑ increase, ←/↓ decrease.
data Nudge = NLeft | NRight | NUp | NDown

derive instance eqNudge :: Eq Nudge

-- | Which number an arrow moves. One arrow moves one axis — never both — so a
-- | caller transmitting relative edits sends exactly one input per keystroke.
data Axis = Beats | Steps

derive instance eqAxis :: Eq Axis

dirOf :: String -> Maybe Nudge
dirOf = case _ of
  "ArrowLeft" -> Just NLeft
  "ArrowRight" -> Just NRight
  "ArrowUp" -> Just NUp
  "ArrowDown" -> Just NDown
  _ -> Nothing

axisOf :: Nudge -> Axis
axisOf = case _ of
  NLeft -> Steps
  NRight -> Steps
  NUp -> Beats
  NDown -> Beats

-- | Apply one arrow. ←/→ resize the ring (n), ↑/↓ fill or empty it (k); beats
-- | stay inside steps, so shrinking the ring carries the pulse count down with it
-- | rather than leaving an impossible k > n behind.
nudge :: Bounds -> Nudge -> Boolean -> Euclid -> Euclid
nudge b dir shift e = case dir of
  NRight -> resize (e.steps + d)
  NLeft -> resize (e.steps - d)
  NUp -> e { beats = clamp 0 e.steps (e.beats + d) }
  NDown -> e { beats = clamp 0 e.steps (e.beats - d) }
  where
  d = if shift then b.coarse else 1
  resize n =
    let n' = clamp b.minSteps b.maxSteps n
    in { steps: n', beats: clamp 0 n' e.beats }

-- | The same keystroke as a **relative** edit: which axis moves, and by how much
-- | (signed, already clamped — so at a limit the amount is 0 and the caller sends
-- | nothing). For callers whose model is elsewhere and takes relative inputs;
-- | Odonus does, because a burst of edits is buffered for the rig and an absolute
-- | `current ± 1` would re-read a stale value.
stepOf :: Bounds -> Nudge -> Boolean -> Euclid -> { axis :: Axis, amount :: Int }
stepOf b dir shift e =
  let e' = nudge b dir shift e
  in case axisOf dir of
    Beats -> { axis: Beats, amount: e'.beats - e.beats }
    Steps -> { axis: Steps, amount: e'.steps - e.steps }

-- | Is step `i` a pulse of E(k, n)? The Bresenham rule the reef engine plays —
-- | see the note on `hit` in the module header before changing this.
hit :: Euclid -> Int -> Boolean
hit e i
  | e.beats <= 0 = false
  | e.beats >= e.steps = true
  | otherwise = ((i `mod` e.steps) * e.beats) `mod` e.steps < e.beats

bits :: Euclid -> Array Boolean
bits e = map (hit e) (range 0 (max 1 e.steps - 1))

-- ---------------------------------------------------------------------------
-- Styling
-- ---------------------------------------------------------------------------

-- | How the figure is drawn. `fill` is the machine's or voice's colour; `ink` is
-- | the centre label and the playhead outline. `inset` sets how far the dot ring
-- | sits inside the box, `stretch` whether the svg fills its container or holds
-- | `size` px exactly.
type Style =
  { size :: Number
  , inset :: Number
  , dotOn :: Number
  , dotOff :: Number
  , fill :: String
  , ink :: String
  , fontSize :: Number
  , label :: Boolean
  , stretch :: Boolean
  }

defaultStyle :: Style
defaultStyle =
  { size: 72.0
  , inset: 10.0
  , dotOn: 3.8
  , dotOff: 2.4
  , fill: "#3f6f8a"
  , ink: "#2b2922"
  , fontSize: 14.0
  , label: true
  , stretch: true
  }

-- | The selectable box's two appearances. Kept as raw css so each machine keeps
-- | its own surface — Selene's slots float on white, Odonus's sit on a voice strip.
type Chrome = { idle :: String, active :: String }

defaultChrome :: Chrome
defaultChrome =
  { idle: "background:#ffffff55;border:1px solid #00000010;"
  , active: "background:#ffffffcc;border:1px solid #1a1a1a;box-shadow:0 0 0 1px #1a1a1a;"
  }

-- | The selection contract: is this the selected ring, what to raise on click,
-- | and what to raise on a keystroke while it holds focus.
type Select i =
  { selected :: Boolean
  , onSelect :: i
  , onKey :: KeyboardEvent -> i
  }

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

r2 :: Number -> Number
r2 x = toNumber (round (x * 100.0)) / 100.0

-- | The ring: one dot per step round the circle (12 o'clock = step 0, clockwise),
-- | filled where E(k,n) pulses, `k/n` at the centre. `playhead` outlines the step
-- | the voice is on — `Nothing` for a figure with no transport of its own.
ring :: forall w i. Style -> Maybe Int -> Euclid -> HH.HTML w i
ring st playhead e =
  let
    c = st.size / 2.0
    r = c - st.inset
    n = max 1 e.steps
    live i = case playhead of
      Just p -> i == p `mod` n
      Nothing -> false
    dotFor i =
      let
        ang = (toNumber i / toNumber n) * 2.0 * pi - pi / 2.0
        on = hit e i
      in
        svgEl "circle"
          [ svgAttr "cx" (show (r2 (c + r * cos ang)))
          , svgAttr "cy" (show (r2 (c + r * sin ang)))
          , svgAttr "r" (show (if on then st.dotOn else st.dotOff))
          , svgAttr "fill" (if on then st.fill else "none")
          , svgAttr "stroke" (if live i then st.ink else st.fill)
          , svgAttr "stroke-width" (if live i then "1.6" else (if on then "0" else "1"))
          ] []
    centre =
      if not st.label then []
      else
        [ svgEl "text"
            [ svgAttr "x" (show c), svgAttr "y" (show (c + st.fontSize / 3.0))
            , svgAttr "text-anchor" "middle", svgAttr "fill" st.ink
            , svgAttr "font-family" "'SF Mono',Menlo,monospace"
            , svgAttr "font-size" (show (round st.fontSize))
            ]
            [ HH.text (show (min e.beats n) <> "/" <> show n) ]
        ]
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show st.size <> " " <> show st.size)
      , svgAttr "width" (if st.stretch then "100%" else show st.size)
      , svgAttr "height" (show st.size)
      , svgAttr "style" "display:block"
      ]
      (map dotFor (range 0 (n - 1)) <> centre)

-- | The whole control: a focusable box that selects on click and takes arrows
-- | while selected. `tabIndex 0` is what makes the keystroke arrive at all — the
-- | box has to be able to hold focus, so the click both selects the ring in the
-- | model and focuses the element in the DOM.
cell :: forall w i. Style -> Chrome -> Select i -> Maybe Int -> Euclid -> HH.HTML w i
cell st chrome sel playhead e =
  HH.div
    [ HP.tabIndex 0
    , HE.onClick \_ -> sel.onSelect
    , HE.onKeyDown sel.onKey
    , HP.attr (HH.AttrName "style") $
        "width:" <> show (round st.size) <> "px;flex:0 0 auto;padding:4px;border-radius:7px;"
          <> "display:flex;flex-direction:column;align-items:center;cursor:pointer;outline:none;"
          <> (if sel.selected then chrome.active else chrome.idle)
    ]
    [ ring st playhead e ]
