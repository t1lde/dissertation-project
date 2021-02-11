module Parse.Base where

import Data.Void
import Data.List as L
import Control.Monad
import Text.Megaparsec hiding (ParseError)
import Data.Text

-- Common imports for Parse submodules

type Parser a = Parsec Void Text a
type ParseError = ParseErrorBundle Text Void

type ParseResult a = Either ParseError a


(<++>) :: (Applicative f, Semigroup a) => (f a) -> (f a) -> (f a)
(<++>) x y = (<>) <$> x <*> y
