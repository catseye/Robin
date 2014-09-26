> module Robin.Parser (parseRobin, insistParse) where

> import Data.Char
> import Data.Int

> import Text.ParserCombinators.Parsec

> import Robin.Expr

Parser
======

The overall grammar of the language is:

    Expr    ::= (symbol | number | boolean | "(" {Expr} ")")
    Program ::= {Expr}

A symbol is denoted by a string which may contain only alphanumeric
characters and certain other characters.

(TODO: this set of characters is provisional.  It might be easier to specify
which characters are *not* allowed.)

> legalSymbolic = (char '*' <|> char '-' <|> char '/' <|>
>                  char '+' <|> char '<' <|> char '>' <|>
>                  char '<' <|> char '=' <|> char '?' <|>
>                  char '_' <|> char '!' <|> char '$' <|>
>                  char ':' <|> char '@')

> symbol = do
>     c <- (letter <|> legalSymbolic)
>     cs <- many (alphaNum <|> legalSymbolic)
>     return (Symbol (c:cs))

TODO: document these productions.

> number = do
>     c <- digit
>     cs <- many digit
>     num <- return (read (c:cs) :: Int32)
>     return (Number num)

> boolean = do
>     string "#"
>     c <- (char 't' <|> char 'f')
>     return (if c == 't' then (Boolean True) else (Boolean False))

> list = do
>     string "("
>     spaces
>     many comment
>     e <- many expr
>     string ")"
>     return $ List e

> stringSugar = do
>     string "'"
>     sentinel <- many $ satisfy (\x -> x /= '\'')
>     string "'"
>     contents <- many $ satisfy (\x -> x /= '\'')
>     string "'"
>     (try $ stringTail sentinel contents) <|> (stringCont sentinel contents)

> stringCont sentinel contents = do
>     contents' <- many $ satisfy (\x -> x /= '\'')
>     let contents'' = contents ++ "'" ++ contents'
>     string "'"
>     (try $ stringTail sentinel contents'') <|> (stringCont sentinel contents'')

> stringTail sentinel contents = do
>     string sentinel
>     string "'"
>     return $ List (map charToNum contents)
>  where
>     charToNum x = Number (fromIntegral $ ord x)

> comment = do
>     string ";"
>     spaces
>     expr

The top-level parsing function implements the overall grammar given above.
Note that we need to give the type of this parser here -- otherwise the
type inferencer freaks out for some reason.

> expr :: Parser Expr
> expr = do
>     r <- (symbol <|> number <|> boolean <|> list <|> stringSugar)
>     spaces
>     many comment
>     return r

> robinProgram = do
>     spaces
>     many comment
>     e <- many expr
>     return $ e

Convenience functions for parsing Robin programs.

> parseRobin = parse robinProgram ""

> insistParse programText =
>     let
>         Right ast = parseRobin programText
>     in
>         ast