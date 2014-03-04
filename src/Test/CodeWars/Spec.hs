module Test.CodeWars.Spec (spec) where
import Data.ByteString.Char8 (pack, unpack)
import System.Environment (withArgs)
import qualified Data.Knob 
import System.IO (hClose, IOMode( WriteMode ))
import Test.CodeWars.Formatter (json)
import Data.String.Utils (replace)
import Test.Hspec (Spec)
import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter,configHandle))

spec :: Spec -> IO String
spec s = withArgs [] $ do
  knob <- Data.Knob.newKnob (pack [])
  h <- Data.Knob.newFileHandle knob "Test.CodeWars" WriteMode
  _ <- hspecWith defaultConfig {configFormatter = json
                               , configHandle = Left h} s
  hClose h
  bytes <- Data.Knob.getContents knob
  let out = replace ",}" "}" $ unpack bytes
  return out
