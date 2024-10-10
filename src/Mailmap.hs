module Mailmap (normalise, parse, invert) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Text.Parsec (manyTill, anyChar, char, try, skipMany, anyToken, lookAhead)
import qualified Text.Parsec as Parsec
import Control.Applicative
import Text.Parsec.Text (Parser)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Coerce
import GHC.IsList
import Prettyprinter (Pretty, Doc)
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text as Pretty

normalise :: Text -> Text
normalise txt
  = Pretty.renderStrict
  $ Pretty.layoutPretty Pretty.defaultLayoutOptions
  $ Pretty.pretty
  $ either (error "Parse error") id
  $ Parsec.parse parseMailmap mempty txt

parse :: Text -> Either Parsec.ParseError (Mailmap String)
parse = Parsec.parse parseMailmap mempty

space :: Parser Char
space = char ' '

spaces :: Parser ()
spaces = skipMany space

bracketed :: Char -> Char -> Parser String
bracketed b e = char b *> anyTill (char e)

anyTill :: (Parsec.Stream s m a, Show a) => Parsec.ParsecT s u m end -> Parsec.ParsecT s u m [a]
anyTill = manyTill anyToken

eol :: Parser Char
eol = char '\n'

parseEmails :: Parser (NonEmpty String)
parseEmails = fmap NonEmpty.fromList $ bracketed '<' '>' `Parsec.sepBy1` spaces

parseMailmap :: Parser (Mailmap String)
parseMailmap = fromList <$> parseLines

parseLines :: Parser [(String, NonEmpty String)]
parseLines = many $ parseLine <* eol

parseLine :: Parser (String, NonEmpty String)
parseLine = do
  name <- manyTill anyChar (try (spaces *> lookAhead (char '<')))
  emails <- parseEmails
  pure (name, emails)

newtype Mailmap a = Mailmap (Map a (Set (a, a)))

deriving newtype instance Show a => Show (Mailmap a)

instance Ord a => IsList (Mailmap a) where
  type Item (Mailmap a) = (a, NonEmpty a)
  fromList :: [(a, NonEmpty a)] -> Mailmap a
  fromList = Mailmap . Map.fromListWith mappend . foldMap go
    where
    go :: (a, NonEmpty a) -> [(a, Set (a, a))]
    go (n, e :| []) = pure (e, Set.singleton (n, e))
    go (n, e :| es) = fmap gogo es
      where
      gogo :: a -> (a, Set (a, a))
      gogo e' = (e, Set.singleton (n, e'))
  toList :: Mailmap a -> [Item (Mailmap a)]
  toList = error "Unimplemented"

instance (Pretty a, PrintfType a, PrintfArg a, Eq a) => Pretty (Mailmap a) where
  pretty :: Mailmap a -> Doc ann
  pretty = Pretty.vcat . fmap Pretty.pretty . foldMap go . Map.toList . coerce
    where
    go :: (a, Set (a, a)) -> [a]
    go (e, xs) = step <$> Set.toList xs
      where
      step (n, e')
        | e == e'   = printf "%s <%s>" n e
        | otherwise = printf "%s <%s> <%s>" n e e'

invert :: Ord b => Map a [b] -> Map b [a]
invert = Map.fromListWith mappend . foldMap swap . Map.toList
  where
  swap (a, bs) = (\b -> (b, [a])) <$> bs
