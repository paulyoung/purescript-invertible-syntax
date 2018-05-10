module Text.Syntax.Classes where

import Prelude hiding (($), (<*>), apply, map)

import Control.Isomorphism.Partial (class IsoFunctor)
import Data.Tuple (Tuple)


class ProductFunctor f where
  apply :: forall a b. f a -> f b -> f (Tuple a b)

infixr 6 apply as <*>


class Alternative f where
  alt :: forall a. f a -> f a -> f a
  empty :: forall a. f a

infixl 3 alt as <|>


class (IsoFunctor d, ProductFunctor d, Alternative d) <= Syntax d where
  pure :: forall a. Eq a => a -> d a
  token :: d Char
