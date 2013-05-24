This Cakeulator module parses a string into its constituent values. This is
useful for I/O and the eval operation.

> module CakeParser ( parse
>                   ) where

Imports, as always.

> import CakeStack ( Stack ( Stack
>                          )
>                  )
> import qualified CakeValue as V
> import Data.Char ( isDigit
>                  , isSpace
>                  )
> import Data.Either.Unwrap ( fromLeft
>                           , fromRight
>                           , isLeft
>                           , isRight
>                           )
> import Data.List ( span
>                  )
> import Data.Word ( Word8
>                  )

The parsing function takes a string and returns a stack of values.

> parse :: String -> Either String (Stack V.Value)

It's just a forwarder for the recursive parser in normal mode.

> parse s
>   | isLeft ret  = Left $ fromLeft $ ret
>   | isRight ret = Right $ Stack $ fromRight ret
>   where
>     ret = normalParse s []

The recursive parser's signature is also simple.

> normalParse :: String -> [V.Value] -> Either String [V.Value]

An empty string means we have succeeded.

> normalParse "" stk = Right stk

Whitespace is skipped.

> normalParse (c:cs) stk
>   | isSpace c = normalParse cs stk

Sequences of digits are parsed as integers (as far as possible).

> normalParse (c:cs) stk
>   | isDigit c   = normalParse rst $ (V.CInteger int):stk
>   where
>     (dgts, rst) = span (isDigit) (c:cs)
>     int         = (read dgts) :: Integer

An opening parenthesis starts a string. Push an empty one.

> normalParse ('(':cs) stk = stringParse 1 cs $ (V.CParen []):stk

Any of the operators push the respective operation.

> normalParse ('+':cs) stk = normalParse cs $ (V.CAdd):stk
> normalParse ('-':cs) stk = normalParse cs $ (V.CSubtract):stk
> normalParse ('*':cs) stk = normalParse cs $ (V.CMultiply):stk
> normalParse ('/':cs) stk = normalParse cs $ (V.CDivide):stk
> normalParse ('%':cs) stk = normalParse cs $ (V.CModulus):stk
> normalParse ('&':cs) stk = normalParse cs $ (V.CAnd):stk
> normalParse ('|':cs) stk = normalParse cs $ (V.COr):stk
> normalParse ('=':cs) stk = normalParse cs $ (V.CEqual):stk
> normalParse ('<':cs) stk = normalParse cs $ (V.CLessThan):stk
> normalParse ('>':cs) stk = normalParse cs $ (V.CGreaterThan):stk
> normalParse ('~':cs) stk = normalParse cs $ (V.CNegate):stk
> normalParse ('!':cs) stk = normalParse cs $ (V.CCopy):stk
> normalParse ('#':cs) stk = normalParse cs $ (V.CDelete):stk
> normalParse ('@':cs) stk = normalParse cs $ (V.CEval):stk
> normalParse ('?':cs) stk = normalParse cs $ (V.CChar):stk
> normalParse (';':cs) stk = normalParse cs $ (V.CAppend):stk
> normalParse ('$':cs) stk = normalParse cs $ (V.COutput):stk

The double quote switches to controlled parsing mode.

> normalParse ('"':cs) stk = controlledParse [] cs stk

Anything else puzzles the parser.

> normalParse (x:xs) stk = Left ("unknown token: " ++ [x])

The controlled parser contains an additional parameter accumulating the string.

> controlledParse :: V.String8 -> String -> [V.Value] -> Either String [V.Value]

If the controlled parser hits an empty string, we have succeeded.

> -- variant which tosses last string not terminated with a newline
> controlledParse _xs "" stk = Right stk
> -- variant which stores it
> --controlledParse xs "" stk = Right $ (V.CParen xs):stk

If the controlled parser hits a newline, it stores the string as a parenthesized
expression.

> controlledParse xs ('\n':cs) stk = controlledParse [] cs $ (V.CParen xs):stk

If the controlled parser hits an apostrophe, it switches back to normal mode.

> -- variant which tosses last string not terminated with a newline
> controlledParse _xs ('\'':cs) stk = normalParse cs stk
> -- variant which stores it
> --controlledParse xs ('\'':cs) stk = normalParse cs $ (V.CParen xs):stk

Otherwise, it appends the character to the running string if it is valid.

> controlledParse xs (c:cs) stk
>   | (i < 0) || (i > 255) = Left ("invalid character found (codepoint " ++ show i ++ ")")
>   | otherwise            = controlledParse (x:xs) cs stk
>   where
>     i = fromEnum c
>     x = toEnum i

The string parser needs a "depth" parameter because parenthesized expressions
may be nested.

> stringParse :: Integer -> String -> [V.Value] -> Either String [V.Value]

If the string parser hits an empty string, cry havoc -- a string has not been
terminated.

> stringParse d "" stk = Left $ "unterminated string (nesting depth " ++ (show d) ++ ")"

If it hits an opening parenthesis, append it and continue parsing with increased
depth.

> stringParse d ('(':cs) stk = stringParse (d+1) cs $ appendToHead (cToS8 '(') stk

If the string parser hits a closing parenthesis, reduce the depth by one. If it
is greater than zero, continue parsing the string; if it is zero, switch back to
the normal parser.

> stringParse 1 (')':cs) stk = normalParse cs stk
> stringParse d (')':cs) stk
>   | d < 1     = Left "unexpected string parsing depth"
>   | otherwise = stringParse (d - 1) cs $ appendToHead (cToS8 ')') stk

Otherwise, append the character to the top-of-the-stack string.

> stringParse d (c:cs) stk
>   | (i < 0) || (i > 255) = Left ("invalid character found (codepoint " ++ show i ++ ")")
>   | otherwise            = stringParse d cs $ appendToHead [toEnum i] stk
>   where
>     i = fromEnum c

The append-to-head operation looks as follows:

> appendToHead :: [Word8] -> [V.Value] -> [V.Value]
> appendToHead add ((V.CParen old):xs) = ((V.CParen (old ++ add)):xs)

This helper function converts characters into single-element Word8 strings:

> cToS8 :: Char -> [Word8]
> cToS8 c
>   | (ci > 255) = error "character out of range"
>   | otherwise  = [toEnum $ fromEnum c]
>   where
>     ci = fromEnum c
