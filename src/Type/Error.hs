module Type.Error where

import Util.Pretty
import qualified AST.AST as AST
import qualified Type.Types as Types

import Data.List.NonEmpty
import Data.List
import qualified Data.Text as T

data TypeError = TypeError AST.TermLoc ErrInfo
instance Pretty TypeError where
  prettyShow (TypeError loc err) = (prettyShow loc) <> ": " <> (prettyShow err)

data ErrInfo
  = NotInScope AST.TermIdentifier     -- Free variable
  | UnknownType AST.TermIdentifier    -- No type can be inferred
  | Reused AST.TermIdentifier         -- Linear variable reuse
  | NotUsed (NonEmpty AST.TermIdentifier)      -- Linear variables not used
  | Mismatch Types.Type Types.Type    -- Different Types
  | WrongSide Types.SessionType       -- Mismatch of send/recv
  | NoTail Types.SessionType          --
  | NotFunction Types.Type Types.Type            -- Expected function type
  | NotName                           -- Term is not a name
  | NameConflict AST.TermIdentifier   -- Multiple uses of same name in let tuple
  | NotSession Types.Type             -- Excepted Session Type
  | NotFork Types.Type
  | CannotDispose Types.Type
  | Malformed

-- Trivial Eq/Ord instances needed by error reporting
instance Eq ErrInfo where
  a == b = True
instance Ord ErrInfo where
  a <= b = True

instance Pretty ErrInfo where
  prettyShow (NotInScope name) = "variable " <> (prettyShow name) <> " is not in scope."
  prettyShow (UnknownType name) = "bound variable " <> (prettyShow name) <> " is not given a type."
  prettyShow (Reused name) = "bound variable " <> (prettyShow name) <> " is used more than once."
  prettyShow (NotUsed (ids)) = T.intercalate "\n" $ fmap (\name -> "bound variable " <> (prettyShow name) <> " is not used.") (toList ids)
  prettyShow (Mismatch actual expected) = "Couldn't match expected type " <> (prettyShow expected) <> " with actual type " <> (prettyShow actual) <> " ."
  prettyShow (WrongSide s@(Types.Send _ _)) = "Communication Mismatch: Trying to recieve from channel of type " <> (prettyShow s) <> " which expects to be send a value."
  prettyShow (WrongSide r@(Types.Recv _ _)) = "Communication Mismatch: Trying to send over channel of type " <> (prettyShow r) <> "which expects to recieve a value."
  prettyShow (WrongSide _) = ""
  prettyShow (NoTail st) = "Cannot send/recieve with channel of type  " <> (prettyShow st) <> " - waiting to be closed."
  prettyShow (NotFunction t arg) = "Couldn't match expected type " <> (prettyShow arg) <> " -@ t with actual type" <> (prettyShow t) <> "."
  prettyShow (NotName) = "Expected a name in variable binder"
  prettyShow (NameConflict name) = "Name " <> (prettyShow name) <> " is already bound inside by let binding."
  prettyShow (NotSession t) = "Couldn't match expected session type with actual type " <> (prettyShow t) <> " ."
  prettyShow (NotFork t) = "Couldn't match expected type Session -> End! with actual type " <> (prettyShow t) <> " ."
  prettyShow (Malformed) = "Malformed AST: This Term is not expected here!"
  prettyShow (CannotDispose t) = "Cannot Dispose/Print type: " <> (prettyShow t) <> " ."
