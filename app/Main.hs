module Main where

import System.Environment
import Control.Monad
import qualified Data.Text.IO as T
import Data.Text

import GHC.IO.Encoding

import Data.Maybe

-- TODO: Obscure program structure and export as necessary in Lib.hs

import GVLib
import Text.Megaparsec hiding (ParseError)
import Data.List.NonEmpty

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

getFileArg :: MaybeT IO FilePath
getFileArg = MaybeT $ fmap listToMaybe getArgs

handleErr :: Monad m => (a -> m ()) -> Either a b -> MaybeT m b
handleErr hand (Right x) = return x
handleErr hand (Left x) = (lift $ hand x) >> mzero

displayParseErr :: ParseError -> IO ()
displayParseErr = T.putStrLn . prettyShow

handleTypeErr :: (NonEmpty TypeError -> IO ()) -> [TypeError] -> MaybeT IO ()
handleTypeErr hand [] = handleErr hand (Right ())
handleTypeErr hand (e:es) = handleErr hand (Left (e :| es))

displayTypeErrs :: FilePath -> Text -> NonEmpty TypeError -> IO ()
displayTypeErrs fn src errs = T.putStr $ prettyShowErrs errs fn src

mainApp :: MaybeT IO ()
mainApp = do
  filename <- getFileArg
  source <- liftIO $ T.readFile filename
  program <- handleErr displayParseErr (parseProgram filename source)
  liftIO $ T.putStrLn $ prettyShow program
  handleTypeErr (displayTypeErrs filename source) (checkProgram program)
  liftIO $ T.putStrLn "Passed Typechecking"
  let runtime = runProgram program
  liftIO $ T.putStrLn $ prettyShow runtime
  return ()

main ::  IO ()
main = do
  runMaybeT mainApp
  return ()


mainApp' :: MaybeT IO ()
mainApp' = do
  let filename = "examples/builtins.sgv"
  source <- liftIO $ T.readFile filename
  program <- handleErr displayParseErr (parseProgram filename source)
  liftIO $ T.putStrLn $ prettyShow program
  handleTypeErr (displayTypeErrs filename source) (checkProgram program)
  liftIO $ T.putStrLn "Passed Typechecking"
  let (runtimeResult) = runProgram program
  liftIO $ T.putStrLn $ prettyShow runtimeResult
  return ()
