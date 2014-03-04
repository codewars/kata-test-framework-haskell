module Test.CodeWars.Formatter (json) where
import Test.Hspec.Formatters (silent,
                              Formatter (..),
                              FormatM,
                              write,
                              getRealTime,
                              getCPUTime,
                              getFailCount,
                              formatException)
import Data.Aeson.Encode (encode)
import Data.ByteString.Lazy.Char8  (unpack)
import GHC.Exception (SomeException)
import Text.Printf (printf)

-- TODO: Push changes upstream to HSpec to get use Text rather than Strings
json :: Formatter
json =
  silent {
   headerFormatter = do
     write "{"
     write $ quote "output" ++ " : { "
       
 , exampleGroupStarted = \_ _ name ->
     write $ quote name ++ " : { "

 , exampleSucceeded = \(_, requirement) -> do
     write $ quote requirement ++ " : { "
     writePair "success" "true"
     write "},"
     
 , exampleFailed = \(_, requirement) reason -> do
     write $ quote requirement ++ " : { "
     writePair "success" "false"
     writePair "reason" (err reason)
     write "},"
     
 , exampleGroupDone = write "},"
 
 , footerFormatter = do
       write "},"
       n <- getFailCount
       writePair "success" (if n == 0 then "true" else "false")
       time <- getRealTime
       writePair "time" (showTime time)
       maybeCPUTime <- getCPUTime
       maybe (return ()) (writePair "CPUtime" . showTime) maybeCPUTime
       write "}"
    }

showTime :: Double -> String
showTime = printf "%f"

writePair :: String -> String -> FormatM ()
writePair k v = write $ quote k ++ " : " ++ v ++ ","

quote :: String -> String
quote = unpack . encode

err :: Either SomeException String -> String
err reason = quote $ either (("uncaught exception: " ++) . formatException) id reason
