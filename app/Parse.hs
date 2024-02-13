{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parse () where

import Lang hiding (getPos)
import Text.Parsec hiding (parse, runP)
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
      reservedNames = ["send", "fun", "trx", "run", "from", "to", "max", "remaining", "src", "dst", "coin"],
      reservedOpNames = [":", "%", "@", ";", "/"]
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
      p <- perc
      reserved "from"
      SPerc p <$> srcs
  )
    <|> ( do
            reserved "max"
            n <- num
            reserved "from"
            SMax n <$> srcs
        )
    <|> ( SAcc <$> identifier
        )
    <|> ( do
          reserved "remaining"
          reserved "from"
          SRem <$> srcs
        )

srcs :: P (Srcs String)
srcs =
  ( do
      a <- identifier
      return [SAcc a]
  )
    <|> braces (many src)

dst :: P (Dst String)
dst =
  ( do
      p <- perc
      reserved "to"
      DPerc p <$> dsts
  )
    <|> DAcc
    <$> identifier
    <|> ( do
          reserved "remaining"
          reserved "to"
          DRem <$> dsts
        )

dsts :: P (Dsts String)
dsts =
  ( do
      a <- identifier
      return [DAcc a]
  )
    <|> braces (many dst)

send :: P STerm
send = do
  reserved "send"
  s <- srcs
  c <- coin
  SSend s c <$> dsts

param :: P (Param String)
param = try (PS <$> srcs) <|> try (PD <$> dsts) <|> PC <$> coin

run :: P STerm
run = do
  reserved "run"
  s <- identifier
  SRun s <$> many param

trx :: P STerm
trx = do
  reserved "trx"
  STrx <$> braces terms

arg :: P (String, Ty)
arg = do
  s <- identifier
  ( do
      reserved "src"
      return (s, TS)
    ) <|> ( do
      reserved "dst"
      return (s, TD)
    ) <|> ( do
      reserved "coin"
      return (s,TC)
    )

fun :: P STerm
fun = do
  reserved "fun"
  s <- identifier
  a <- arg
  SFun s a <$> braces terms

term :: P STerm
term = send <|> fun <|> trx <|> run

terms :: P [STerm]
terms = many term

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP term s "" of
  Right t -> t
  Left e -> error ("no parse: " ++ show s)