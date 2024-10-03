{-# language OverloadedStrings #-}
module Main (main) where

import Test.QuickCheck
import qualified Mailmap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Printf
import Control.Monad
import Control.Applicative

idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f x = f (f x) == f x

main :: IO ()
main = quickCheck $ forAll gLines $ idempotent Mailmap.normalise

email :: Gen Text
email = do
  n <- chooseInt (0, 6)
  pure $ Text.pack $ printf "<example+%d@example.org>" n

name :: Gen Text
name = do
  n <- chooseInt (0, 6)
  pure $ Text.pack $ printf "person-%d" n

line :: Gen Text
line = do
  n <- name
  k <- chooseInt (1, 4)
  es <- replicateM k email
  pure $ Text.unwords $ n:es
  
gLines :: Gen Text
gLines = do
  k <- chooseInt (0, 40)
  ls <- replicateM k line
  pure $ Text.unlines ls
