-- examples from Daan Leijen, 2001, Parsec, a fast combinator parser
-- section references are to the User Guide portion of
-- http://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.pdf
-- adapted, where necessary, to conform to requirements under GHC 7.10.3
-- I highly recommend that you work the examples in conjunction with the text of the User Guide
-- by Richard Careaga (@technocrat on Twitter and github) 2016-01-02

-- pragmas added
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Userguide where

-- imports updated and added
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec as PC -- qualified to avoid
                                                     -- ambiguity of "try"
                                                     -- with Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String
import Text.Parsec.Token as P
--import qualified ParsecToken as P     omitted, no longer in Parsec
--import ParsecLanguage( haskellStyle   omitted, no longer in Parsec

-- Section 2.1
simple :: Parser Char
simple  = letter

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

{-
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> :l Userguide
[1 of 1] Compiling Userguide        ( Userguide.hs, interpreted )
Ok, modules loaded: Userguide.
*Userguide> run simple "a"
'a'
*Userguide> run simple ""
parse error at (line 1, column 1):
unexpected end of input
expecting letter
*Userguide> run simple "123"
parse error at (line 1, column 1):
unexpected "1"
expecting letter
*Userguide>
-} 

-- Section 2.2
openClose :: Parser Char
openClose = do { char '('
               ; char ')'
               }

parens'  :: Parser ()    -- parens (no ') now conflicts with Text.Parsec.Token
parens'  = do { char '('
              ; parens'
              ; char ')'
              ; parens'
              }
         <|> return ()

{- 
*Userguide> run parens "(())()"
()
*Userguide> run parens "(()()"
parse error at (line 1, column 6):
unexpected end of input
expecting "(" or ")"
-} 

-- Section 2.3
-- type declaration now required for Monomorphism Restriction
testOr :: Stream s m Char => ParsecT s u m String
testOr =   string "(a)"
       <|> string "(b)"

{- 
*Userguide> run testOr "(b)"
parse error at (line 1, column 1):
unexpected "b"
expecting "(a)"
-} 

-- unclear whether this example is intended to return only the closing ')'
-- type declaration now required for Monomorphism Restriction
testOr1 :: Stream s m Char => ParsecT s u m Char
testOr1 = do { char '('
          ; char 'a' <|> char 'b'
          ; char ')'
          }

{- 
*Userguide> run testOr1 "(b)"
')'
-} 

-- type declaration now required for Monomorphism Restriction
testOr2 :: Stream s m Char => ParsecT s u m String
testOr2 = try (string "(a)")
        <|> string "(b)"

{-
*Userguide> run testOr2 "(b)"
"(b)"
-} 

-- type declaration now required for Monomorphism Restriction
testOr3 :: Stream s m Char => ParsecT s u m String
testOr3 =   do { try (string "(a"); char ')'
               ; return "(a)" 
               }
               <|> string "(b)"

{- 
*Userguide> run testOr3 "(b)"
"(b)"
-} 

-- Section 2.4
nesting :: Parser Int
nesting = do { char '('
             ; n <- nesting
             ; char ')'
             ; m <- nesting 
             ; return (max (n+1) m)
             }
             <|> return 0

{- 
*Userguide> run nesting "(())()"
2
*Userguide> run nesting "(()(()))"
3
*Userguide> run nesting "(()(())"
parse error at (line 1, column 8):
unexpected end of input
expecting "(" or ")"
-} 

-- Section 2.5
word    :: Parser String
word    = do { c  <- letter
             ; do { cs <- word
                  ; return (c:cs)
                  }
             <|> return [c]
             }

{- 
*Userguide> run word "example"
"example"
*Userguide> run word "e"
"e"
-} 

-- Section 2.6
-- renamed word' to avoid conflict with example function word (no apostrophe)
word'    :: Parser String
word'    = many1 letter

{- 
*Userguide> run word' "example"
"example"
*Userguide> run word' "e"
"e"
-} 

-- words' renamed to avoid shadowing words (no apostrophe )
sentence :: Parser [String]
sentence = do { words' <- sepBy1 word separator
              ; oneOf ".?!"
              ; return words'
              }

separator :: Parser ()
separator = skipMany1 (space <|> char ',')

{-
*Userguide> run sentence "hi,di,hi."
["hi","di","hi"]
*Userguide> run sentence "hi,di hi!"
["hi","di","hi"]
*Userguide> run sentence "hi,123"
parse error at (line 1, column 4):
unexpected "1"
expecting space, "," or letter
-} 

-- renamed word'' to avoid conflict with example function word' (one apostrophe)
word''    :: Parser String
word''    = many1 letter <?> "word"

{- 
*Userguide>  run sentence "hi,123"
parse error at (line 1, column 4):
unexpected "1"
expecting space, "," or letter
-} 

-- renamed separator' to avoid conflict with example function separator (no apostrophe)
separator'   :: Parser ()
separator'   = skipMany1 (space <|> char ',' <?> "")

-- renamed sentence' to avoid conflict with example function sentence (no apostrophe)
sentence'    :: Parser [String]
sentence'    = do { words <- sepBy1 word separator' 
                  ; oneOf ".?!" <?> "end of sentence"
                  ; return words
                  }
{- 
*Userguide> run sentence' "hi,di"
parse error at (line 1, column 6):
unexpected end of input
expecting space, "," or end of sentence
-} 

-- renamed word''' to avoid conflict with example function word'' (two apostrophes)
word'''    :: Parser String
word'''    = many1 (letter <?> "") <?> "word"

-- renamed sentence'' to avoid conflict with example function sentence (one apostrophe)
sentence''    :: Parser [String]
sentence''    = do { words <- sepBy1 word''' separator' 
                  ; oneOf ".?!" <?> "end of sentence"
                  ; return words
                  }
{-
*Userguide> run sentence' "hi di"
parse error at (line 1, column 6):
unexpected end of input
expecting end of sentence
*Userguide> run sentence'' "hi di,"
parse error at (line 1, column 7):
unexpected end of input
expecting word
-} 

-- Section 2.7
expr    :: Parser Integer
expr    = buildExpressionParser table factor
        <?> "expression"

table   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
          ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ string s; return f}) assoc

factor  = do { char '('
             ; x <- expr
             ; char ')'
             ; return x
             }
        <|> number
        <?> "simple expression"

number  :: Parser Integer
number  = do { ds <- many1 digit
             ; return (read ds)
             }
        <?> "number"

{-
*Userguide> run expr "1+2*3"
7
*Userguide> run expr "(1+2)*3"
9
*Userguide> run expr "8/4/2"
1
*Userguide> run expr "8/(4/2)"
4
*Userguide> run expr "1 + 2"
1
*Userguide> run expr "1+ 2"
parse error at (line 1, column 3):
unexpected " "
expecting simple expression
-} 

-- Section 2.8
lexer :: TokenParser ()
lexer = do { makeTokenParser
             (haskellDef
               { reservedOpNames = ["*","/","+","-"]
               }
             )
            }

whiteSpace' = P.whiteSpace lexer -- added apostrophe to avoid ambiguous occurrence

lexeme'     = P.lexeme lexer  -- added apostrophe to avoid ambiguous occurrence

symbol'     = P.symbol lexer  -- added apostrophe to avoid ambiguous occurrence

natural'    = P.natural lexer -- added apostrophe to avoid ambiguous occurrence

parens''     = P.parens lexer -- added apostrophes to avoid ambiguous occurrence

semi'        = P.semi lexer   -- added apostrophe to avoid ambiguous occurrence

identifier' = P.identifier lexer -- added apostrophe to avoid ambiguous occurrence

reserved'   = P.reserved lexer   -- added apostrophe to avoid ambiguous occurrence

reservedOp' = P.reservedOp lexer -- added apostrophe to avoid ambiguous occurrence

-- renamed expr' to avoid conflict with example function expr (no apostrophe)
expr'    :: Parser Integer
expr'    = buildExpressionParser table' factor' -- conforming change
        <?> "expression"

-- renamed table' to avoid conflict with example function table (no apostrophe)
table'   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
           ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
           ]
        where
          op s f assoc
             = Infix (do
                      { reservedOp' s; return f}
                      <?> "operator"
                      ) assoc

factor'  = parens'' expr'
         <|> natural'
         <?> "simple expression"

runLex :: Show a => Parser a -> String -> IO ()
runLex p input
        = run (do { whiteSpace'
                  ; x <- p
                  ; eof
                  ; return x
                  }
              ) input

{-
*Userguide> runLex expr' "1 + 2" -- note: expr', not expr, all in this block
3
*Userguide> runLex expr' "1 + {- comment -} 2 * 3 --multiply has higher priority"
7
*Userguide> runLex expr' "  0xAA / 0o37 / 2"
2
*Userguide> runLex expr' "0xAA / 0o37 2 "
parse error at (line 1, column 13):
unexpected '2'
expecting operator or end of input
-}

-- Section 2.9.1
price :: Parser Int   -- price in cents
price = lexeme' ( do { ds1 <- many1 digit
                    ; char '.'
                    ; ds2 <- count 2 digit
                    ; return (convert 0 (ds1 ++ ds2))
                    }
                )
       <?> "price"
       where
         convert n [] =n
         convert n (d:ds) = convert (10*n + digitToInt d) ds 

receipt :: Parser Bool
receipt = do { ps <- many produkt
             ;p <-total
             ; return (sum ps == p) 
             }

produkt = do { symbol' "return"
             ; p <- price
             ; semi'
             ; return (-p)
             }
          <|> do { identifier'
              ; p <-price
              ; semi'
              ; return p
              }
          <?> "produkt"           

total = do { p <- price
           ; symbol' "total"
           ; return p 
           }

{-
*Userguide> runLex receipt "book 12.00; plant 2.55; 14.55 total"
True
*Userguide> runLex receipt "book 12.00; plant 2.55; 12.55 total"
False
*Userguide> runLex receipt "book 12.00; plant 2; 12.55 total"
parse error at (line 1, column 20):
unexpected ";"
expecting digit or "."
*Userguide> runLex receipt "book 12.00; return 2.00; plant 2.55; 12.55 total"
True
*Userguide> runLex receipt "book 12.00; reader 2.00; plant 1.00; 15.00 total"
parse error at (line 1, column 13):
unexpected "a"
expecting "return"
*Userguide>
-} 

-- Section 2.9.2
-- renamed receipt' to avoid conflict with example function receipt (no apostrophe)
receipt' :: Parser Bool
receipt' = do { ps <- many produkt'
             ;p <-total
             ; return (sum ps == p) 
             }

-- renamed produkt' to avoid conflict with example function produkt (no apostrophe)
produkt' = do { try (symbol' "return")
              ; p <- price
              ; semi'
              ; return (-p)
              }
        <|> do { identifier' ;p <-price
               ; semi'
               ; return p
               }
        <?> "produkt'"

{- 
*Userguide> runLex receipt' "book 12.00; reader 2.00; plant 1.00; 15.00 total"
True
*Userguide> runLex receipt' "book 12.00; returns 2.00; plant 1.00; 15.00 total"
parse error at (line 1, column 19):
unexpected "s"
expecting price
-} 

-- renamed lexer' to avoid conflict with example function lexer (no apostrophe)
lexer' :: TokenParser ()
lexer' = makeTokenParser
         (haskellDef
           { reservedNames   = ["return","total"]
           , reservedOpNames = ["*","/","+","-"]
           }
          )

-- conforming changes

whiteSpace'' = P.whiteSpace lexer' 

lexeme''     = P.lexeme lexer'

symbol''     = P.symbol lexer'

natural''    = P.natural lexer'

parens'''    = P.parens lexer'

semi''        = P.semi lexer'

identifier'' = P.identifier lexer'

reserved''   = P.reserved lexer'

reservedOp'' = P.reservedOp lexer'

receipt'' :: Parser Bool
receipt'' = do { ps <- many produkt''
               ;p <-total
               ; return (sum ps == p) 
               }

produkt'' = do { reserved'' "return"
               ; p <- price
               ; semi''
               ; return (-p)
               }
         <|> do { identifier''
                ;p <-price
                ; semi''
                ; return p
                }
         <?> "produkt''"

total'' = do { p <- price
             ; reserved'' "total"
             ; return p
             } 

{- note receipt'' (double apostrophe)
*Userguide> runLex receipt'' "book 12.00; returns 2.00; plant 1.00; 15.00 total"
True
*Userguide> runLex receipt'' "book 12.00; total 2.00; plant 1.00; 15.00 total"
parse error at (line 1, column 18):
unexpected reserved word "total"
expecting produkt''
*Userguide> runLex receipt'' "book 12.00; totals 2.00; return 1.00; 13.00 total"
True
-} 

-- Section 2.11
type Token  = (SourcePos,Tok)
data Tok    = Identifier String
            | Reserved String
            | Symbol String
            | Price Int
            deriving Show

scanner :: [Char] -> ([Token],[String])
scanner = undefined -- no definition given in the User Guide

type MyParser a   = GenParser Token () a

-- added do
mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test = do token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

identifier :: MyParser String
identifier = mytoken (\tok -> case tok of
                       Identifier name -> Just name
                       other-> Nothing)

reserved :: String -> MyParser ()
reserved name = mytoken (\tok -> case tok of
                          Reserved s | s == name  -> Just ()
                          other -> Nothing
                        )

-- Section 2.12
-- renamed run' to avoid conflict with example function run (no apostrophe)
run' input
  = case runParser parser 0 "" input of
      Right n  -> putStrLn ("counted " ++ show n ++ " identifiers!")
      Left err -> do { putStr "parse error at "
                     ; print err
                     }

parser :: CharParser Int Int
parser
  = do { undefined -- elipses (...) given in original
       ; n <- getState
       ; return n 
       }

-- renamed lexer'' to avoid conflict with example functions lexer and lexer'
lexer'' = P.makeTokenParser haskellDef

-- renamed identifier''' to avoid conflict with example functions 
-- identifier' and identifier''
identifier'''  = P.identifier lexer''

myIdentifier :: CharParser Int String
myIdentifier = do { x <- identifier'''
                  ; updateState (+1)
                  ; return x
                  }

{-
No ghci examples were given with these definitions following type Token
-} 

-- Section 2.13
-- rewritten to provide type signature and update syntax
perm0 :: Parser String 
perm0 = permute (f <$$> char 'a'
                    <||> char 'b'
                    <||> char 'c'
                )
      where
        f a b c = [a,b,c]

{- 
*Userguide> run perm0 "abc"
"abc"
*Userguide> run perm0 "cba"
"abc"
*Userguide> run perm0 "b"
parse error at (line 1, column 2):
unexpected end of input
expecting "c" or "a"
-} 
