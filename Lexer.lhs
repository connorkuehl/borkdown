> module Lexer where
>
> import Prelude hiding (lex)
>
> data Token = Text Char
>            | Ast
>            | AstAst
>            | Und
>            | UndUnd
>            | Head Int
>   deriving Show
>
> lex    :: String -> [Token]
> lex [] = []
> lex s  = fst lexed:(lex (snd lexed))
>   where lexed = tokenize s
>
> tokenize                  :: String -> (Token, String)
> tokenize ('\\':'*':chars) = (Text '*',      chars)
> tokenize ('*':'*':chars)  = (AstAst,        chars)
> tokenize ('*':chars)      = (Ast,           chars)
> tokenize ('\\':'_':chars) = (Text '_',      chars)
> tokenize ('_':'_':chars)  = (UndUnd,        chars)
> tokenize ('_':chars)      = (Und,           chars)
> tokenize ('#':chars)      = (Head h, drop h chars)
>   where 
>     h         = length header
>     header    = heading ('#':chars)
>     heading   = takeWhile isHead
>     isHead ch = ch == '#'
> tokenize (ch:chars)       = (Text ch, chars)
