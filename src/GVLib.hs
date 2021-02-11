module GVLib
  ( module Util.Pretty
  , module AST.AST
  , module Parse.Parse
  , parseProgram
  , checkProgram
  , testParse
  , prettyShowErrs
  , runProgram
  , module Type.Error
  , module Parse.Base
  ) where

import Text.Megaparsec hiding (parse)
import Data.Void
import Data.Text
import Data.Text.IO as T
import Data.Either

import Util.Pretty
import Util.ErrorReporting
import AST.AST (Term, AST, TermLoc)
import Parse.Parse
import Parse.Base

import Parse.AST
import Parse.TopLevel
import Type.Checker
import Type.Check
import Type.Types
import Type.Error

import Execution.Substitution
import Execution.Runtime
import Execution.Reduction

import Data.Generics.Fixplate


testParse = T.putStrLn $ showResult $ runParser (parse :: Parser TopLevel) "test" testInput
  where
    testInput =
      "let x = y in (x::test);"
    showResult r = case r of
      (Right result) -> prettyShow result
      (Left err) -> pack $ errorBundlePretty err

parseProgram :: String -> Text -> ParseResult Program
parseProgram filename input = mapP (uniqueNames :: AST -> AST) <$> parseFile filename input
  where


checkProgram :: Program -> [TypeError]
checkProgram (Program p) = lefts $ checkTerms p
  where
    checkTerms :: [TopLevel] -> [Either TypeError Type]
    checkTerms p = fst . runChecker <$> (programTerms p)

runProgram :: Program -> [(Runtime, Text)]
runProgram (Program ts) = reduceProgram <$> prepRuntime ts
  where
    prepRuntime :: [TopLevel] -> [Runtime]
    prepRuntime =  (fmap toRuntime) . programTerms
