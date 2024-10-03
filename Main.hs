module Main (main) where

import qualified Mailmap
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  t <- Text.getContents
  Text.putStr $ Mailmap.normalise t
