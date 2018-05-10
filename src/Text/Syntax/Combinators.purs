-- | Derived combinators
module Text.Syntax.Combinators where

import Prelude hiding ((<$>),(<*>),(<*),(*>),pure,unit)

import Control.Isomorphism.Partial (Iso, inverse, (<$>))
import Control.Isomorphism.Partial.Constructors (cons, left, nil, right)
import Control.Isomorphism.Partial.Derived (foldl)
import Control.Isomorphism.Partial.Prim (commute, element, ignore, unit)
import Data.Either (Either)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..))
import Data.String (uncons)
import Data.Tuple (Tuple(..))
import Data.Unit as Unit
import Text.Syntax.Classes (class Syntax, pure, token, (<*>), (<|>))

-- Lexemes

-- | `text` parses/prints a fixed text and consumes/produces a unit value.
text :: forall d. Syntax d => String -> d Unit
text s
  | Just { head: c, tail: s' } <- uncons s =
      inverse (element (Tuple Unit.unit Unit.unit))
        <$> (inverse (element c) <$> token)
        <*> text s'
  | otherwise = pure Unit.unit

comma :: forall d. Syntax d => d Unit
comma = text ","

dot :: forall d. Syntax d => d Unit
dot = text "."

-- Repetition

many :: forall d a. Syntax d => d a -> d (List a)
many p = nil <$> pure Unit.unit <|> cons <$> p <*> many p

many1 :: forall d a. Syntax d => d a -> d (List a)
many1 p = cons <$> p <*> many p

sepBy :: forall d a. Syntax d => d a -> d Unit -> d (List a)
sepBy x sep = nil <$> text "" <|> cons <$> x <*> many (sep *> x)

-- | The `chainl1` combinator is used to parse a left-associative chain of
-- | infix operators.
chainl1
  :: forall d a b
   . Syntax d
  => d a
  -> d b
  -> Iso (Tuple a (Tuple b a)) a
  -> d a
chainl1 arg op f = foldl f <$> arg <*> many (op <*> arg)

-- Sequencing

-- | This variant of `<*>` ignores its left result. In contrast to its
-- | counterpart derived from the `Apply` class, the ignored parts have type
-- | `d Unit` rather than `d b` because otherwise information relevant for
-- | pretty-printing would be lost.
applySecond :: forall d a. Syntax d => d Unit -> d a -> d a
applySecond p q = inverse unit <<< commute <$> p <*> q

infixl 9 applySecond as *>

-- | This variant of `<*>` ignores its right result. In contrast to its
-- | counterpart derived from the `Apply` class, the ignored parts have type
-- | `d Unit` rather than `d b` because otherwise information relevant for
-- | pretty-printing would be lost.
applyFirst :: forall d a. Syntax d => d a -> d Unit -> d a
applyFirst p q = inverse unit <$> p <*> q

infixl 9 applyFirst as <*

-- | The `between` function combines `*>` and `<*` in the obvious way.
between :: forall d a. Syntax d => d Unit -> d Unit -> d a -> d a
between p q r = p *> r <* q

-- Alternation

alt :: forall d a b. Syntax d => d a -> d b -> d (Either a b)
alt p q = (left <$> p) <|> (right <$> q)

infixl 4 alt as <+>

-- optional ::
-- optional

-- Whitespace

-- | Expressing whitespace
-- |
-- | Parsers and pretty printers treat whitespace differently. Parsers specify
-- | where whitespace is allowed or required to occur, while pretty printers
-- | specify how much whitespace is to be inserted at these locations. To
-- | account for these different roles of whitespace, the following three syntax
-- | descriptions provide fine-grained control over where whitespace is allowed,
-- | desired or required to occur.

-- | `skipSpace` marks a position where whitespace is allowed to occur. It
-- | accepts arbitrary space while parsing, and produces no space while
-- | printing.
skipSpace :: forall d. Syntax d => d Unit
skipSpace = ignore Nil <$> many (text " ")

-- | `optSpace` marks a position where whitespace is desired to occur. It
-- | accepts arbitrary space while parsing, and produces a single space
-- | character while printing.
optSpace :: forall d. Syntax d => d Unit
optSpace = ignore (singleton Unit.unit) <$> many (text " ")

-- | `sepSpace` marks a position where whitespace is required to occur. It
-- | requires one or more space characters while parsing, and produces a single
-- | space character while printing.
sepSpace :: forall d. Syntax d => d Unit
sepSpace = text " " <* skipSpace
