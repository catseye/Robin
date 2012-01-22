> module Robin.Parser (parseRobin, insistParse) where

> import Data.Ratio

> import Text.ParserCombinators.Parsec

> import Robin.Expr

Parser
======

The overall grammar of the language is:

    Expr ::= (symbol | number | boolean | "(" {Expr} ["." Expr] ")")

A symbol is denoted by a string which may contain only alphanumeric
characters, hyphens, underscores, and question marks.  (TODO: this set
of characters is provisional.)

> legalSymbolic = (char '*' <|> char '-' <|> char '/' <|>
>                  char '+' <|> char '<' <|> char '>' <|>
>                  char '<' <|> char '=' <|> char '?' <|>
>                  char '_')

> symbol = do
>     c <- (letter <|> legalSymbolic)
>     cs <- many (alphaNum <|> legalSymbolic)
>     return (Symbol (c:cs))

TODO: document these productions.

> number = do
>     c <- digit
>     cs <- many digit
>     num <- return (read (c:cs) :: Integer)
>     fraction num <|> return (Number (num % 1))

> fraction num = do
>     string "/"
>     c <- digit
>     cs <- many digit
>     denom <- return (read (c:cs) :: Integer)
>     return (Number (num % denom))

> boolean = do
>     string "#"
>     c <- (char 't' <|> char 'f')
>     return (if c == 't' then (Boolean True) else (Boolean False))

> proper e = do
>     string ")"
>     return $ robinizeList e Null

> improper e = do
>     string "."
>     spaces
>     e2 <- expr
>     string ")"
>     return $ robinizeList e e2

> list = do
>     string "("
>     spaces
>     e <- many expr
>     proper e <|> improper e

The top-level parsing function implements the overall grammar given above.
Note that we need to give the type of this parser here -- otherwise the
type inferencer freaks out for some reason.

> expr :: Parser Expr
> expr = do
>     r <- (symbol <|> number <|> boolean <|> list)
>     spaces
>     return r

Convenience functions for parsing Robin programs.

> parseRobin = parse expr ""

> insistParse program =
>     let
>         Right ast = parseRobin program
>     in
>         ast
