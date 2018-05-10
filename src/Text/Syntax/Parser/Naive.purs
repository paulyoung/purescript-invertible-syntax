module Text.Syntax.Parser.Naive where

import Prelude hiding (apply)

import Control.Isomorphism.Partial (class IsoFunctor, apply)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String (uncons)
import Data.Tuple (Tuple(..))
import Text.Syntax.Classes (class Alternative, class ProductFunctor, class Syntax)

newtype Parser a = Parser (String -> List (Tuple a String))

instance isoFunctorParser :: IsoFunctor Parser where
  map iso (Parser p) = Parser f where
    f s | Cons (Tuple x s') _ <- p s, Just y <- apply iso x = pure $ Tuple y s'
    f _ = Nil

instance productFunctorParser :: ProductFunctor Parser where
  apply (Parser p) (Parser q) = Parser f where
    f s | Cons (Tuple x s') _ <- p s, Cons (Tuple y s'') _ <- q s' = pure $ Tuple (Tuple x y) s''
    f _ = Nil

instance alternativeParser :: Alternative Parser where
  alt (Parser p) (Parser q) = Parser \s -> p s <> q s
  empty = Parser \_ -> Nil

instance syntaxParser :: Syntax Parser where
  pure x = Parser \s -> pure $ Tuple x s

  token = Parser f where
    f s | Just { head, tail } <- uncons s = pure $ Tuple head tail
    f _ = Nil

parse :: forall a. Parser a -> String -> List a
parse (Parser p) s | Cons (Tuple x "") _ <- p s = pure x
parse _ _ = Nil
