module Parse.Symbols where

import Data.Text

-- Types
unitTypeSymbol :: Text
unitTypeSymbol = "()"
linearFunctionTypeSymbol :: Text
linearFunctionTypeSymbol = "-@"
sumTypeSymbol :: Text
sumTypeSymbol = "+"
productTypeSymbol :: Text
productTypeSymbol = "*"
sendTypeSymbol :: Text
sendTypeSymbol = "!"
recvTypeSymbol :: Text
recvTypeSymbol = "?"
sendEndTypeSymbol :: Text
sendEndTypeSymbol = "End!"
recvEndTypeSymbol :: Text
recvEndTypeSymbol = "End?"
sessionTypeConcatSymbol :: Text
sessionTypeConcatSymbol = "."
dualTypeSymbol :: Text
dualTypeSymbol = "~"

-- Type annotations, declarations and judgements
typeAnnotationSymbol :: Text
typeAnnotationSymbol = "::"
typeDeclarationEqualsSymbol :: Text
typeDeclarationEqualsSymbol = "="
typeJudgementSymbol :: Text
typeJudgementSymbol = "|-"
declEndSymbol :: Text
declEndSymbol = ";"
judgementNameSymbol :: Text
judgementNameSymbol = ":"
judgementSeparatorSymbol :: Text
judgementSeparatorSymbol = ","
keywordType :: Text
keywordType = "type"

--Terms
unitTermSymbol :: Text
unitTermSymbol = "()"
parenL :: Text
parenL = "("
parenR :: Text
parenR = ")"
lambdaSymbol :: Text
lambdaSymbol = "\\"
lambdaDotSymbol :: Text
lambdaDotSymbol = "."
equalsSymbol :: Text
equalsSymbol = "="
caseBracketL :: Text
caseBracketL = "{"
caseBracketR :: Text
caseBracketR = "}"
caseResultSymbol :: Text
caseResultSymbol = "|->"
caseSeparatorSymbol :: Text
caseSeparatorSymbol = ";"
tupleSeparatorSymbol :: Text
tupleSeparatorSymbol = ","

keywordLambda :: Text
keywordLambda = "lambda"
keywordLet :: Text
keywordLet = "let"
keywordOf :: Text
keywordOf = "of"
keywordInL :: Text
keywordInL = "inl"
keywordInR :: Text
keywordInR = "inr"
keywordSend :: Text
keywordSend = "send"
keywordRecieve :: Text
keywordRecieve = "recieve"
keywordWait :: Text
keywordWait = "wait"
keywordFork :: Text
keywordFork = "fork"
keywordCase :: Text
keywordCase = "case"
keywordIn :: Text
keywordIn = "in"

-- Comments
lineCommentSymbol :: Text
lineCommentSymbol = "--"
blockCommentStartSymbol :: Text
blockCommentStartSymbol = "{-"
blockCommentEndSymbol :: Text
blockCommentEndSymbol = "-}"

-- Literals
quoteSymbol :: Text
quoteSymbol = "\""

-- Extra BuiltIn Types
intTypeSymbol :: Text
intTypeSymbol = "Int"
stringTypeSymbol :: Text
stringTypeSymbol = "String"

reservedKeywords :: [Text]
reservedKeywords = [keywordLambda, keywordLet, keywordOf, keywordInL, keywordInR, keywordSend, keywordRecieve, keywordWait, keywordType, keywordFork, keywordType, keywordIn, intTypeSymbol, stringTypeSymbol]
