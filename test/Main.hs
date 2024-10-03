module Main (main) where

import Test.QuickCheck
import qualified Mailmap
import Data.Text (Text)
import qualified Data.Text as Text

idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f x = f (f x) == f x

main :: IO ()
main = quickCheck $ forAll g $ idempotent Mailmap.normalise
  where
  g :: Gen Text
  g = Text.pack <$> arbitrary
