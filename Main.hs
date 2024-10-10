module Main (main) where

import qualified Mailmap
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  t <- Text.getContents
  Text.putStrLn $ Mailmap.normalise t
