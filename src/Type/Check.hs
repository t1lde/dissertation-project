module Type.Check where

import Type.Checker
import Type.Types
import AST.AST
import Parse.TopLevel
import Type.Error


type CheckResult = (Either TypeError Type,  Context)
