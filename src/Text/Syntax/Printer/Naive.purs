module Text.Syntax.Printer.Naive where

import Prelude hiding (apply)

import Control.Alt as Alt
import Control.Apply (lift2)
import Control.Isomorphism.Partial (class IsoFunctor, unapply)
import Data.Maybe (Maybe(..))
import Data.String (singleton)
import Data.Tuple (Tuple(..))
import Text.Syntax.Classes (class Alternative, class ProductFunctor, class Syntax)

newtype Printer a = Printer (a -> Maybe String)

instance isoFunctorPrinter :: IsoFunctor Printer where
  map iso (Printer p) = Printer \b -> unapply iso b >>= p

instance productFunctorPrinter :: ProductFunctor Printer where
  apply (Printer p) (Printer q) = Printer \(Tuple x y) -> lift2 (<>) (p x) (q y)

instance alternativePrinter :: Alternative Printer where
  alt (Printer p) (Printer q) = Printer \s -> Alt.alt (p s) (q s)
  empty = Printer \s -> Nothing

instance syntaxPrinter :: Syntax Printer where
  pure x = Printer f where
    f y | x == y = Just ""
    f _ = Nothing

  token = Printer \t -> Just $ singleton t

print :: forall a. Printer a -> a -> Maybe String
print (Printer p) x = p x
