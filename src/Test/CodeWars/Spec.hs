module Test.CodeWars.Spec (spec) where
import Data.ByteString.Char8 (pack, unpack)
import System.Environment (withArgs)
import qualified Data.Knob 
import System.IO (hClose, IOMode( WriteMode ))
import qualified System.IO as IO 
import Test.CodeWars.Formatter (json,kvPair,quote)
import Data.String.Utils (replace)
import Test.Hspec (Spec)
import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter,configHandle))
import System.IO.Silently (hCapture)

spec :: Spec -> IO String
spec s = withArgs [] $ do
  knob <- Data.Knob.newKnob (pack [])
  h <- Data.Knob.newFileHandle knob "Test.CodeWars" WriteMode
  (stdout, (stderr,_)) <-
    hCapture [IO.stdout] $ hCapture [IO.stderr] $
    hspecWith defaultConfig {configFormatter = json
                            , configHandle = Left h} s
  hClose h
  bytes <- Data.Knob.getContents knob
  let outputJSON = replace ",}" "}" $ unpack bytes
  return $
    "{"
    ++ kvPair "stdout" (quote stdout)
    ++ kvPair "stderr" (quote stderr)
    ++ tail outputJSON
