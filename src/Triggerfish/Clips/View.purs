-- | `Triggerfish.Clips.View` — the shared MIDI clip library, as ONE renderer used
-- | from two places (#28-lib, docs/DESIGN-capture-surface.md):
-- |
-- |   * the SHELL modal (⌥6) — browse/manage from any machine: audition, rename,
-- |     delete. This is the home a captured clip is reachable from now that the
-- |     on-surface clip strip is gone (#28a).
-- |   * a Vetula box's phrase picker — the same rows plus an ＋ attach column.
-- |
-- | Pure and POLYMORPHIC in the host's `action`, exactly like `Capture.View`: this
-- | module never imports a machine's Action type (that would be a cycle, and
-- | machine-specific). The host passes a `LibraryWiring` of the constructors it
-- | wants emitted, and `attach` is the ONLY difference between the two modes —
-- | `Nothing` = browse/manage, `Just` = attach-to-this-box.
-- |
-- | Naming clips is a mug's game (you won't remember what "clip 7" was), so a row
-- | leads with what you can actually recognise it by: which machine captured it,
-- | its key, its tags, and how much is in it.
module Triggerfish.Clips.View
  ( LibraryWiring
  , AttachTo
  , libraryPanel
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Clips (MidiClip, headCount)

-- | The attach column: which voice the ＋ lands on (`label` is shown in the legend,
-- | e.g. "P3") and the constructor that takes the clip.
type AttachTo action = { label :: String, onAttach :: MidiClip -> action }

-- | What the host gives the shared library view. `attach` selects the mode.
type LibraryWiring action =
  { audition :: MidiClip -> action        -- ▶ play once, faithfully (own channels/vel/gate)
  , rename :: String -> String -> action  -- clip id → new name (commit on blur)
  , delete :: String -> action            -- clip id
  , attach :: Maybe (AttachTo action)     -- Nothing = browse/manage; Just = attach column
  }

style :: forall r i. String -> HP.IProp r i
style = HP.attr (HH.AttrName "style")

-- | The whole library surface: a count line, a legend of what the controls do, then
-- | one row per clip (newest first — the store conses on capture).
libraryPanel :: forall action slots m. LibraryWiring action -> Array MidiClip -> H.ComponentHTML action slots m
libraryPanel w clips =
  HH.div_
    ( [ HH.div [ style "display:flex;align-items:baseline;gap:8px;margin-bottom:4px" ]
          [ HH.div [ style "font-size:13px;letter-spacing:0.04em;text-transform:uppercase;color:#7a5c00" ]
              [ HH.text "clip library" ]
          , HH.div [ style "font-size:11px;color:#b0a684" ]
              [ HH.text (show n <> " clip" <> (if n == 1 then "" else "s") <> " · shared across machines") ]
          ]
      , HH.div [ style "font-size:11px;color:#a89a70;margin-bottom:12px" ] [ HH.text legend ]
      ]
        <> (if n == 0 then [ emptyState ] else map clipRow clips)
    )
  where
  n = length clips

  legend = case w.attach of
    Nothing -> "▶ audition · rename inline · × delete from the shared library"
    Just a -> "▶ audition · rename inline · × delete · ＋ attach to " <> a.label
      <> " (a self-contained copy; the transform stack still applies)"

  emptyState =
    HH.div [ style "font-size:12px;color:#b0a684;padding:14px 0" ]
      [ HH.text "No clips yet. Capture one in a machine's REPLAY surface (◆ mark → loop → ⧉ clip)." ]

  -- One library row: a source badge, an inline-editable name (rename on blur), the
  -- computed/stored metadata, then the controls. A div, not a button, so the name
  -- <input> and the control buttons nest legally.
  clipRow c =
    HH.div
      [ style $ "display:flex;align-items:center;gap:8px;width:100%;border:1px solid #e0d6bc;"
          <> "background:#fdfbf5;padding:6px 8px;border-radius:6px;margin-bottom:5px;font-size:12px" ]
      ( [ sourceBadge c.source
        , HH.input
            [ HP.value c.name
            , HP.placeholder "unnamed"
            , HP.title "rename this clip in the shared library"
            , style $ "flex:1 1 auto;min-width:0;border:none;border-bottom:1px dashed #d8cba0;"
                <> "background:transparent;color:#5a4a2a;font-size:12px;padding:1px 2px"
            , HE.onValueChange (w.rename c.id) ]
        , metaSpan (fromMaybe "" c.key)
        , metaSpan (joinWith " " (map ("#" <> _) c.tags))
        , metaSpan (show (headCount c.events) <> "ch")
        , metaSpan (show (length c.events) <> "n")
        , iconBtn "#6a4a8a" "▶" "audition this clip (own channels · velocity · gate)" (w.audition c)
        ]
          <> (case w.attach of
                Just a -> [ iconBtn "#2f7d5a" "＋" "attach a copy as this voice's source" (a.onAttach c) ]
                Nothing -> [])
          <> [ iconBtn "#a44" "×" "delete this clip from the shared library" (w.delete c.id) ]
      )

  -- Which machine captured the clip, as a coloured pill.
  sourceBadge src =
    let col = case src of
          "odonus" -> "#2f7d8a"
          "vetula" -> "#6a4a8a"
          "balistes" -> "#8a5a2a"
          _ -> "#9a9070"
    in HH.span
      [ style $ "font-size:9px;letter-spacing:0.04em;text-transform:uppercase;color:#fff;background:"
          <> col <> ";padding:1px 5px;border-radius:3px;white-space:nowrap"
      , HP.title ("captured in " <> src) ]
      [ HH.text src ]

  metaSpan t =
    if t == "" then HH.text ""
    else HH.span [ style "font-size:10px;color:#a89a70;white-space:nowrap" ] [ HH.text t ]

  iconBtn col glyph tip act =
    HH.button
      [ style $ "border:1px solid #dcd2b4;background:#faf6ea;color:" <> col
          <> ";cursor:pointer;padding:2px 6px;border-radius:4px;font-size:12px;line-height:1;white-space:nowrap"
      , HP.title tip
      , HE.onClick \_ -> act ]
      [ HH.text glyph ]
