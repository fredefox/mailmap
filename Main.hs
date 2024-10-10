module Main (main) where

import qualified Mailmap
import qualified Data.Text.IO as Text
import qualified Control.Exception

main :: IO ()
main = do
  t <- Text.getContents
  case Mailmap.normalise t of
    Left e -> Control.Exception.throwIO e
    Right s -> Text.putStrLn s
