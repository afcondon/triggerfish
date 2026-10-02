-- | **Lepidoptera writes a scale by Tidal's name, and its pattern, and reads
-- | them back.** `scale: D dorian "<dorian mixolydian>/4"` loads the named
-- | scale as the one to return to and the pattern as Odonus's scale pattern;
-- | printing gives the same text. A name Tidal does not have is refused, and a
-- | scale with no name keeps its steps.
module Test.LepidopteraScaleSpec (runLepidopteraScaleTests) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), split)
import Data.String.Regex (replace) as Re
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (log)
import Reef.Odonus (defaultOdonus)
import Test.Assert (assertEqual', assertTrue')
import Triggerfish.Odonus.Lepidoptera (parsePatch, printPatch)

runLepidopteraScaleTests :: Effect Unit
runLepidopteraScaleTests = do
  log "Lepidoptera: scales by name and the scale pattern"
  let
    base = { name: "t", odo: defaultOdonus, gen: [], genSpread: 0.5, genBias: 0.5, swing: 0.0, velHumanize: 0, stepDiv: 1 }
    txt = printPatch base
    line2 t = fromMaybe "" (split (Pattern "\n") t !! 1)
    withScale s = Re.replace (unsafeRegex "scale: C [^\\n]*" noFlags) ("scale: " <> s) txt
  assertEqual' "the default prints as C minor" { actual: line2 txt, expected: "  { scale: C minor" }
  case parsePatch (withScale "D dorian \"<dorian mixolydian>/4\"") of
    Nothing -> assertTrue' "a named scale with a pattern reads" false
    Just p -> do
      assertEqual' "root" { actual: p.odo.rootPc, expected: 2 }
      assertEqual' "the named scale is the one returned to" { actual: p.odo.scaleHeld, expected: Just [ 0, 2, 3, 5, 7, 9, 10 ] }
      assertEqual' "the pattern" { actual: p.odo.scalePattern, expected: Just "<dorian mixolydian>/4" }
      assertEqual' "prints as written" { actual: line2 (printPatch p), expected: "  { scale: D dorian \"<dorian mixolydian>/4\"" }
      assertEqual' "round trip" { actual: map printPatch (parsePatch (printPatch p)), expected: Just (printPatch p) }
  assertTrue' "an unknown name is refused" (isNothing (parsePatch (withScale "D dorain")))
  assertTrue' "a microtonal scale is refused" (isNothing (parsePatch (withScale "D bayati")))
  assertEqual' "a scale with no name keeps its steps"
    { actual: line2 <<< printPatch <$> parsePatch (withScale "E [ 0, 1, 7 ]"), expected: Just "  { scale: E [ 0, 1, 7 ]" }
  log "   ok"
