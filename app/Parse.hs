module Parse (parse, runP, term, terms, parseFile) where

import Lang hiding (getPos)
import Common
import Text.Parsec hiding (parse, runP, sepBy1)
import Text.Parsec (sepBy1, many1, try, optional)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Prelude hiding (const)

type P = Parsec String ()

lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef =
  emptyDef
    { commentLine = "--",
      reservedNames = ["send", "trx", "from", "to", "max", "remaining", "account"],
      reservedOpNames = ["%", "@", ";", "/", ","]
    }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

braces :: P a -> P a
braces = Tok.braces lexer

brackets :: P a -> P a
brackets = Tok.brackets lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

num :: P Int
num = fromInteger <$> natural

coin :: P Coin
coin = brackets $
  do
    c <- identifier
    Coin c <$> num

perc :: P Perc
perc =
  try
    ( do
        n <- num
        reservedOp "%"
        return $ Perc n
    )
    <|> ( do
            p <- num
            reservedOp "/"
            Rat p <$> num
        )

src :: P (Src String)
src =
  ( do
      SSingle <$> identifier
  )
    <|> braces ( do
            conns <- sepBy1 srcConnector (reservedOp ",")
            return $ SMultiple conns
        )

srcConnector :: P (SrcConnector String)
srcConnector =
  ( do
            p <- perc
            reserved "from"
            SConnPerc p <$> src
        )
    <|> ( do
            reserved "max"
            n <- num
            reserved "from"
            SConnMax n <$> src
        )
    <|> ( do
            reserved "remaining"
            reserved "from"
            SConnRem <$> src
        )


dst :: P (Dst String)
dst =
  ( do
      DSingle <$> identifier
  )
    <|> braces ( do
            conns <- sepBy1 dstConnector (reservedOp ",")
            return $ DMultiple conns
        )

dstConnector :: P (DstConnector String)
dstConnector =
  ( do
  p <- perc
  reserved "to"
  DConnPerc p <$> dst)
    <|> ( do
            reserved "max"
            n <- num
            reserved "to"
            DConnMax n <$> dst
        )
    <|> ( do
            reserved "remaining"
            reserved "to"
            DConnRem <$> dst
          )


send :: P STerm
send = do
  reserved "send"
  s <- src
  c <- coin
  SSend s c <$> dst

trx :: P STerm
trx = do
  reserved "trx"
  STrx <$> braces terms

account :: P STerm
account = do
  reserved "account"
  accountName <- identifier
  coinName <- identifier
  SAccount accountName coinName . fromInteger <$> natural

-- | Parse a term
term :: P STerm
term = send <|> trx <|> account

terms :: P [STerm]
terms = term `sepBy1` reservedOp ";"

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- | Parse a file containing multiple statements (newline-separated)
parseFile :: String -> Either ParseError [STerm]
parseFile content = runP fileTerms content "file"
  where
    fileTerms :: P [STerm]
    fileTerms = many1 (term <* optional newline)

-- para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP term s "" of
  Right t -> t
  Left e -> error ("no parse: " ++ show s)