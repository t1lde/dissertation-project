module Execution.Reduction where



import Data.Coerce
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List (partition, uncons)
import Data.Maybe
import Data.Bifunctor

import Debug.Trace
import Control.Applicative
{---------------------------------------
  External package imports
---------------------------------------}
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Generics.Fixplate
import Data.Text hiding (zip, foldl', length, null, tail, take, uncons, splitAt, head, mapAccumL)
import qualified Data.Stream.Infinite as S
{-------------------------
  Package imports
-------------------------}
import AST.AST
import AST.Builtins
import AST.Transformations

import Execution.Runtime
import Execution.Substitution

import Util.Pretty


type Rule = Runtime -> RunState (Maybe Runtime)

infixl 3 <|+>

-- Operator to combine reduction rules, returning the result of the first
-- Maybe result, or Nothing if there is none
(<|+>) :: Rule -> Rule -> Rule
(<|+>) rx ry term = (<|>) <$> (rx term) <*> (ry term)


infixr 1 />
(/>) body repl = subst body repl
infixr 2 =:
name =: repl = (name, unFix repl)


execRuntime :: RunState Runtime -> (Runtime, Text)
execRuntime rt = second (envPrint) (runState rt initEnv)

-- Wrap top-level term as necessary for full evaluation
initTerm :: Runtime -> Runtime
initTerm term = (inject $ EvalContext (inject $ Para (term :| [])))

-- Recursive rewriting transform, while this is applied bottom-up
-- the reduction is guarded on EvalContext terms, making the rewrite
-- effectively top-down
reduceProgram :: Runtime -> (Runtime, Text)
reduceProgram term =  execRuntime $ rewriteM reduceRT (initTerm term)

-- Non-recursive rewriting transformation, to be applied bottom-up
-- Nothing signifies normal form
-- using NameGen monad to provide fresh channel names
reduceRT :: Rule
reduceRT term = rLift term

evalCtx :: Runtime -> Runtime
evalCtx term = fromMaybe (fromMaybe (inject $ EvalContext term) (matchVal term)) (matchCtx term)
  where
    matchVal :: Runtime -> Maybe Runtime
    matchVal term = do
      (Value _) <- match term
      return term
    matchCtx :: Runtime -> Maybe Runtime
    matchCtx term = do
      (EvalContext ctx) <- match term
      return term


maybeEval :: Runtime -> Maybe Runtime
maybeEval term = case (match term) of
  (Just (Value _)) -> Nothing
  _ -> case (match term) of
    (Just (EvalContext _)) -> Nothing
    _ -> return (inject $ EvalContext term)

mapMaybeEval :: NonEmpty Runtime -> Maybe (NonEmpty Runtime)
mapMaybeEval ts = (uncurry toMaybe) $ mapAccumL go False ts
  where
    go :: Bool -> Runtime -> (Bool, Runtime)
    go True t = (True, evalCtx t)
    go False t = maybe (False, t) (True,) (maybeEval t)

    toMaybe :: Bool -> a -> Maybe a
    toMaybe False _ = Nothing
    toMaybe True x = Just x

-- Returns a list of all non-failing results of a function in the list
-- paired with the rest of the list with the given item removed
removeMatches :: (a -> Maybe b) ->  [a] ->  [(b, [a])]
removeMatches f xs = ((first f) <$> listOthers xs) >>= filterMatches
  where
    -- head and tail are unsafe :^(
    listOthers :: [a] -> [(a, [a])]
    listOthers xs = [(\(l,r) -> (head r, l <> tail r)) (splitAt i xs) | i <- [0 .. (length xs - 1)]]

    -- Technically safer, but really makes errors invisible as the empty list...
    listOthers' :: [a] -> [(a, [a])]
    listOthers' [] = []
    listOthers' xs = fromMaybe [] $ traverse uncons (take len $ take len <$> iterate tail (cycle xs)) where len = length xs

    filterMatches :: (Maybe b, [a]) -> [(b, [a])]
    filterMatches (Just x, ys) = return $ (x,ys)
    filterMatches _ = mzero

val :: Runtime -> Runtime
val term = (inject $ Value term)

matchVal :: Runtime -> Maybe Runtime
matchVal term = do
  (Value _) <- match term
  return term


------------------------------
-- Lambda Term Rules
------------------------------

-- Guard evaluation on an EvalContext
rLift :: Rule
rLift term =  ((fmap evalCtx) . join) <$> mapM (\x -> (rTerm <|+> rPara <|+> rVal <|+> rEval) x) (matchCtx term)
  where
    matchCtx :: Runtime -> Maybe Runtime
    matchCtx term = do
      (EvalContext ctx) <- match term
      return ctx

-- Propogates EvalContext downwards to terms that are able to be evaluated
rEval :: Rule
rEval term = (evalPara <|+> evalApp <|+> evalTuple <|+> evalLet <|+> evalInL <|+> evalInR <|+> evalInL <|+> evalCase <|+> evalFork <|+> evalSend <|+> evalRecieve <|+> evalWait) term
  where
    evalApp :: Rule
    evalApp = evalAppL <|+> evalAppR

    evalAppL :: Rule
    evalAppL term = return $ do
      (Apply l r) <- match term
      evalL <- maybeEval l
      return $ (inject $ Apply (evalL) r)

    evalAppR term = return $ do
      (Apply l r) <- match term
      (Value _) <- match l
      evalR <- maybeEval r
      return $ (inject $ Apply l (evalR))

    evalTuple :: Rule
    evalTuple term = return $ do
      (Tuple ts) <- match term
      ts' <- mapMaybeEval ts
      return $ inject $ Tuple ts'

    evalLet :: Rule
    evalLet term = return $ do
      (Let xs vs body) <- match term
      evalVs <- maybeEval vs
      return $ inject $ Let xs evalVs body

    evalInL :: Rule
    evalInL term = return $ do
      (InL x) <- match term
      evalX <- maybeEval x
      return $ inject $ InL evalX

    evalInR :: Rule
    evalInR term = return $ do
      (InR x) <- match term
      evalX <- maybeEval x
      return $ inject $ InR evalX

    evalCase :: Rule
    evalCase term = return $ do
      (Case c l r) <- match term
      evalC <- maybeEval c
      return $ inject $ Case (evalC) l r

    evalFork :: Rule
    evalFork term = return $ do
      (Fork f) <- match term
      evalF <- maybeEval f
      return $ inject $ Fork evalF

    evalSend :: Rule
    evalSend = evalSendR <|+> evalSendL

    evalSendL :: Rule
    evalSendL term = return $ do
      (Send l r) <- match term
      evalL <- maybeEval l
      return $ inject $ Send evalL r

    evalSendR term = return $ do
      (Send l r) <- match term
      (Value _) <- match l
      evalR <- maybeEval r
      return $ inject $ Send l evalR

    evalRecieve term = return $ do
      (Recieve r) <- match term
      evalR <- maybeEval r
      return $ inject $ Recieve evalR

    evalWait term = return $ do
      (Wait w) <- match term
      evalW <- maybeEval w
      return $ inject $ Wait evalW

    evalPara term = return $ do
      (Para ts) <- match term
      ts' <- mapMaybeEval ts
      return $ inject $ Para ts'



-- Marks terms which have been evaluated
rVal :: Rule
rVal term =
  (varVal
    <|+>  channelVal
    <|+>  lambdaVal
    <|+>  unitVal
    <|+>  tupleVal
    <|+>  inLVal
    <|+>  inRVal
    <|+> literalVal
    <|+> partialVal) term
  where
    varVal :: Rule
    varVal term = return $ do
      (Id name) <- match term
      return $ val term

    literalVal :: Rule
    literalVal = intVal <|+> stringVal
      where
        intVal :: Rule
        intVal term = return $ do
          (IntL _) <- match term
          return $ val term

        stringVal :: Rule
        stringVal term = return $ do
          (StringL _) <- match term
          return $ val term

    partialVal :: Rule
    partialVal = unaryPartialVal <|+> binaryPartialVal

    unaryPartialVal :: Rule
    unaryPartialVal term = return $ do
      (UnaryFn _ _) <- match term
      return $ val term

    binaryPartialVal :: Rule
    binaryPartialVal term = return $ do
      (BinaryFn _ _ _) <- match term
      return $ val term

    channelVal :: Rule
    channelVal term = return $ do
      (Const (ChanId _)) <- match term
      return $ val term

    lambdaVal :: Rule
    lambdaVal term =  return $ do
      (Lambda _ _) <- match term
      return $ val term

    unitVal :: Rule
    unitVal term =  return $ do
      (UnitVal) <- match term
      return $ val term

    tupleVal :: Rule
    tupleVal term = return $ do
      (Tuple vs) <- match term
      traverse matchVal vs
      return $ val term

    inLVal :: Rule
    inLVal term = return $ do
      (InL v) <- match term
      (Value _) <- match v
      return $ val term

    inRVal term =  return $ do
      (InR v) <- match term
      (Value _) <- match v
      return $ val term


-- Base Term Reduction Rules
rTerm :: Rule
rTerm term = (rLambda
  <|+> rLetUnit
  <|+> rLetTuple
  <|+> rCaseL
  <|+> rCaseR
  <|+> rFork
  <|+> rPartial
  ) term

rLambda :: Rule
rLambda term = return $ do
  (Apply fn arg) <- match term
  (Value argV) <- match arg
  (Value fnVal) <- match fn
  (Lambda name body) <- match fnVal
  (Name bound) <- match name
  return $ body /> bound =: arg

rLetUnit :: Rule
rLetUnit term = return $ do
  (Let xs vs body) <- match term
  guard (null xs)
  (Value vsVal) <- match vs
  UnitVal <- match vsVal
  return $ body

rLetTuple :: Rule
rLetTuple term = return $ do
  (Let xs vs body) <- match term
  (Value vsVal) <- match vs
  let args = matchTupleSingle vsVal
  let names = [name | (Just (Name name)) <- match <$> xs]
  guard (length names == length args)
  return $ foldl' (\body' sub -> body' /> sub) body (zip names (unFix <$> toList args))
  where
    matchTupleSingle :: Runtime -> NonEmpty Runtime
    matchTupleSingle term = fromMaybe (term :| []) $ do
      (Tuple args) <- match term
      return args

rCaseL :: Rule
rCaseL term = return $ do
  (Case cs (CaseBranch name body) _) <- match term
  (Value csVal) <- match cs
  (InL arg) <- match csVal
  (Name bound) <- match name
  return $ body /> bound =: arg

rCaseR :: Rule
rCaseR term = return $ do
  (Case cs _ (CaseBranch name body)) <- match term
  (Value csVal) <- match cs
  (InR arg) <- match csVal
  (Name bound) <- match name
  return $ body /> bound =: arg

rFork :: Rule
rFork term = runMaybeT $ MaybeT (matchChild term) >>= spawnChild
  where
    matchChild :: Rule
    matchChild term = return $ do
      (Fork child) <- match term
      (Value _) <- match child
      return child

    spawnChild :: Runtime -> MaybeT RunState Runtime
    spawnChild child = do
      chan <- lift $ newChannel
      lift $ chan +| (inject $ Apply child chan)

-- Builtin functions
rPartial :: Rule
rPartial = rUnaryApp <|+> rBinaryApp2 <|+> rBinaryApp1 <|+> rUnaryComplete <|+> rBinaryComplete

rUnaryApp :: Rule
rUnaryApp term = return $ do
  (Apply fn arg) <- match term
  (Value fnVal) <- match fn
  (Value argVal) <- match arg
  (UnaryFn ufn EmptyArg) <- match fnVal
  return $ inject $ UnaryFn ufn (AppArg argVal)

rBinaryApp1 :: Rule
rBinaryApp1 term = return $ do
  (Apply fn arg) <- match term
  (Value fnVal) <- match fn
  (Value argVal) <- match arg
  (BinaryFn bfn EmptyArg arg2) <- match fnVal
  return $ inject $ BinaryFn bfn (AppArg argVal) arg2


rBinaryApp2 :: Rule
rBinaryApp2 term = return $ do
  (Apply fn arg) <- match term
  (Value fnVal) <- match fn
  (Value argVal) <- match arg
  (BinaryFn bfn arg1@(AppArg _ ) EmptyArg) <- match fnVal
  return $ inject $ BinaryFn bfn arg1 (AppArg argVal)

rUnaryComplete :: Rule
rUnaryComplete = rNeg <|+> rPrint <|+> rDispose

rNeg :: Rule
rNeg term = return $ do
  (UnaryFn IntNeg (AppArg arg)) <- match term
  (IntL argVal) <- match arg
  return $ inject $ IntL (-argVal)

rPrint :: Rule
rPrint term = runMaybeT $ MaybeT (matchPrint term) >>= printTerm
  where
    matchPrint :: Rule
    matchPrint term = return $ do
      (UnaryFn PrintVal (AppArg arg)) <- match term
      return arg

    printTerm :: Runtime -> MaybeT RunState Runtime
    printTerm term = do
      lift $ modify $ writePrint (prettyShow term)
      lift $ newId

rDispose :: Rule
rDispose term = runMaybeT $ MaybeT (matchDispose term) >>= returnId
  where
    matchDispose :: Rule
    matchDispose term = return $ do
      (UnaryFn DisposeVal (AppArg arg)) <- match term
      return arg

    returnId :: Runtime -> MaybeT RunState Runtime
    returnId term = do
      lift newId


rBinaryComplete :: Rule
rBinaryComplete = rPlus <|+> rSub <|+> rTimes <|+> rConcat

rPlus :: Rule
rPlus term = return $ do
  (BinaryFn IntPlus (AppArg arg1) (AppArg arg2)) <- match term
  (IntL int1) <- match arg1
  (IntL int2) <- match arg2
  return $ inject $ IntL (int1 + int2)

rSub :: Rule
rSub term = return $ do
  (BinaryFn IntSub (AppArg arg1) (AppArg arg2)) <- match term
  (IntL int1) <- match arg1
  (IntL int2) <- match arg2
  return $ inject $ IntL (int1 - int2)

rTimes :: Rule
rTimes term = return $ do
  (BinaryFn IntTimes (AppArg arg1) (AppArg arg2)) <- match term
  (IntL int1) <- match arg1
  (IntL int2) <- match arg2
  return $ inject $ IntL (int1 * int2)


rConcat :: Rule
rConcat term = return $ do
  (BinaryFn StringConcat (AppArg arg1) (AppArg arg2)) <- match term
  (StringL str1) <- match arg1
  (StringL str2) <- match arg2
  return $ inject $ StringL (str1 <> str2)
----------------------------------

----------------------------------
-- Parallel Configuration Rules --
----------------------------------

rPara :: Rule
rPara term = (rAddPara <|+> rSendRecv <|+> rWait) term

-- Pull paralell terms out of runtime to paralell terms
rAddPara :: Rule
rAddPara term = runMaybeT $ MaybeT (return (matchPara term)) >>= addPara
  where
    addPara :: (NonEmpty Runtime) -> MaybeT RunState Runtime
    addPara ts = do
      (Env n mparas prints) <- lift get
      paras <- MaybeT $ return $ nonEmpty mparas
      put $ (Env n [] prints)
      return $ inject $ Para (ts <> paras)

    matchPara :: Runtime -> Maybe (NonEmpty Runtime)
    matchPara term = do
      (Para t) <- match term
      return t

rSendRecv :: Rule
rSendRecv term = return $ do
  (Para paras) <- match term
  sends <- nonEmpty $ removeMatches matchSendCtx (toList paras) -- Finds all evaluatable send terms
  listToMaybe $ mapMaybe reduceMatchingRecv (toList sends)      -- Tries to find the matching recieve for all the send terms

  where
    -- Try to find a term waiting to Recieve over a given channel,
    -- Reduces the send/recieve pair when successfull
    reduceMatchingRecv :: ((Runtime, Runtime, ChannelIdentifier) , [Runtime]) -> Maybe (Runtime)
    reduceMatchingRecv ((sendVal, sendTerm, sendChan), paras) = do
      (recvCtx, paras') <- listToMaybe $ removeMatches (matchRecvCtx sendChan) paras
      return $ inject $ Para (sendTerm :| ((recvCtx sendVal) : paras'))

    -- Searches for a sending term inside a context, returns the value being sent,
    -- along with the Context with the Send term replaced, and the channel sending over
    matchSendCtx :: Runtime -> Maybe (Runtime, Runtime, ChannelIdentifier)
    matchSendCtx term = listToMaybe [(sendVal, ctx chanVal, chanId) | (matchSend -> (Just (sendVal, chanVal, chanId)) ,ctx) <- contextList term]

    -- Searches for a recieving term over a given channel inside a context,
    -- returns the context with a hole to insert the sent value.
    matchRecvCtx :: ChannelIdentifier -> Runtime -> Maybe (Runtime -> Runtime)
    matchRecvCtx sendChan term = listToMaybe [(\sendVal -> ctx (inject $ Tuple (sendVal :| (chanVal : [])))) | (matchRecv sendChan -> (Just chanVal) , ctx) <- contextList term]

    -- Matches an evaluatable Send term
    matchSend :: Runtime -> Maybe (Runtime, Runtime, ChannelIdentifier)
    matchSend term = do
      (EvalContext eval) <- match term
      (Send sendVal chanVal) <- match eval
      (Value _) <- match sendVal
      (Value chan) <- match chanVal
      (Const chanId@(ChanId _)) <- match chan
      return $ (sendVal, chanVal, chanId)

    -- Matches an evaluatable Recieve Term
    matchRecv :: ChannelIdentifier -> Runtime -> Maybe (Runtime)
    matchRecv sendChan term = do
      (EvalContext eval) <- match term
      (Recieve chanVal) <- match eval
      (Value chan) <- match chanVal
      (Const chanId@(ChanId _)) <- match chan
      guard (sendChan == chanId)
      return chanVal

rWait :: Rule
rWait term = return $ do
  (Para paras) <- match term
  chans <- nonEmpty $ removeMatches matchChan (toList paras)
  listToMaybe $ mapMaybe reduceMatchingWait (toList chans)
  where
    reduceMatchingWait :: (ChannelIdentifier, [Runtime]) -> Maybe (Runtime)
    reduceMatchingWait (chanId, paras) = do
      (waitTerm , paras') <- listToMaybe $ removeMatches (matchWaitCtx chanId) paras
      return $ inject $ Para (waitTerm :| paras')

    matchWaitCtx :: ChannelIdentifier -> Runtime -> Maybe (Runtime)
    matchWaitCtx chanId term = listToMaybe [(ctx $ inject $ UnitVal) | (matchWait chanId -> True , ctx) <- contextList term]

    matchWait :: ChannelIdentifier -> Runtime -> Bool
    matchWait closeChan term = isJust $ do
      (EvalContext eval) <- match term
      (Wait chanVal) <- match eval
      (Value chan) <- match chanVal
      (Const waitChan@(ChanId _)) <- match chan
      guard (closeChan == waitChan)
      return ()

    matchChan :: Runtime -> Maybe (ChannelIdentifier)
    matchChan term = do
      (Value v) <- match term
      (Const chanId@(ChanId _)) <- match v
      return chanId


paratest :: Runtime
paratest = inject $ Para $ (inject $ EvalContext $ inject $ Value $ inject $ Const $ ChanId 1) :| ((inject $ EvalContext $ inject $ Value $ inject $ Const $ ChanId 2):[])
