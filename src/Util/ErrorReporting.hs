module Util.ErrorReporting where


import Text.Megaparsec
import Data.List.NonEmpty
import Data.Text
import qualified Data.Set as Set
import Control.Monad.Trans.Reader

import AST.AST
import Type.Error
import Util.Pretty

-- Orphan instance because it's only needed in this module ¯\_(ツ)_/¯
instance ShowErrorComponent ErrInfo where
  showErrorComponent err = unpack $ prettyShow err

mkPosState :: FilePath -> Text -> PosState Text
mkPosState filename source = PosState source 0 (initialPos filename) defaultTabWidth ""

mkError :: (TypeError) -> ParseError Text ErrInfo
mkError (TypeError (TermLoc (offset,_)) err) = FancyError offset (Set.singleton $ ErrorCustom err)

toErrorBundle :: (NonEmpty TypeError) -> FilePath -> Text -> (ParseErrorBundle Text ErrInfo)
toErrorBundle errs fn src =
  (ParseErrorBundle (mkError <$> errs) (mkPosState fn src))

prettyShowErrs :: (NonEmpty TypeError) -> FilePath -> Text -> Text
prettyShowErrs errs fn src = pack $ errorBundlePretty (toErrorBundle errs fn src)
