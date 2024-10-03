{-# options_ghc -Wall -Werror #-}
module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Parsec (manyTill, anyChar, char, try, skipMany, anyToken, lookAhead)
import qualified Text.Parsec as Parsec
import Control.Applicative
import Text.Parsec.Text (Parser)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
  t <- Text.getContents
  Text.putStr $ normalise t

normalise :: Text -> Text
normalise txt
  = Text.pack
  $ showMailmap
  $ either (error "Parse error") id
  $ Parsec.parse parseMailmap mempty txt

space :: Parser Char
space = char ' '

spaces :: Parser ()
spaces = skipMany space

bracketed :: Parsec.Stream s m Char => Char -> Char -> Parsec.ParsecT s u m String
bracketed b e = char b *> anyTill (char e)

anyTill :: (Parsec.Stream s m a, Show a) => Parsec.ParsecT s u m end -> Parsec.ParsecT s u m [a]
anyTill = manyTill anyToken

eol :: Parser Char
eol = char '\n'

parseEmails :: Parser (NonEmpty String)
parseEmails = fmap NonEmpty.fromList $ bracketed '<' '>' `Parsec.sepBy1` spaces

parseMailmap :: Parser Mailmap
parseMailmap = mailmap <$> parseLines

parseLines :: Parser [(String, NonEmpty String)]
parseLines = many $ parseLine <* eol

parseLine :: Parser (String, NonEmpty String)
parseLine = do
  name <- manyTill anyChar (try (spaces *> lookAhead (char '<')))
  emails <- parseEmails
  pure (name, emails)

type Mailmap = Map String (Set (String, String))

mailmap :: [(String, NonEmpty String)] -> Mailmap
mailmap = Map.fromListWith mappend . foldMap go
  where
  go :: (String, NonEmpty String) -> [(String, Set (String, String))]
  go (n, e :| []) = pure (e, Set.singleton (n, e))
  go (n, e :| es) = fmap gogo es
    where
    gogo :: String -> (String, Set (String, String))
    gogo e' = (e, Set.singleton (n, e'))

showMailmap :: Mailmap -> String
showMailmap = unlines . foldMap go . Map.toList
  where
  go :: (String, Set (String, String)) -> [String]
  go (e, xs) = fmap step $ Set.toList xs
    where
    step (n, e')
      | e == e'   = printf "%s <%s>" n e
      | otherwise = printf "%s <%s> <%s>" n e e'
