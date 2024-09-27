{-# options_ghc -Wall -Werror #-}
module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Parsec (manyTill, anyChar, char, try, skipMany)
import qualified Text.Parsec as Parsec
import Control.Applicative
import Text.Parsec.Text (Parser)
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Text.Printf

main :: IO ()
main = do
  t <- Text.getContents
  Text.putStr $ normalise t

normalise :: Text -> Text
normalise txt
  = Text.pack
  $ showMailmap
  $ mailmap
  $ rights
  $ fmap (Parsec.parse parser mempty)
  $ Text.lines txt

space :: Parser Char
space = char ' '

spaces :: Parser ()
spaces = skipMany space

parser :: Parser (String, String, Maybe String)
parser = do
  name <- manyTill anyChar (try (spaces *> char '<'))
  email <- manyTill anyChar (char '>')
  alt <- optional $ do
    _ <- try $ manyTill anyChar (char '<')
    manyTill anyChar (char '>')
  pure (name, email, alt)

type Mailmap = Map String (Set (String, String))

mailmap :: [(String, String, Maybe String)] -> Mailmap
mailmap = Map.fromListWith mappend . fmap go
  where
  go :: (String, String, Maybe String) -> (String, Set (String, String))
  go (n, e, e') = (e, Set.singleton (n, fromMaybe e e'))

showMailmap :: Mailmap -> String
showMailmap = concat . fmap go . Map.toList
  where
  go :: (String, Set (String, String)) -> String
  go (e, xs) = unlines $ fmap step $ Set.toList xs
    where
    step (n, e') = printf "%s <%s> <%s>" n e e'
