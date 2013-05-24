This module defines the types of values available to place onto the stack.

> module CakeValue ( String8
>                  , Value ( CAdd
>                          , CAnd
>                          , CAppend
>                          , CChar
>                          , CCopy
>                          , CDelete
>                          , CDivide
>                          , CEqual
>                          , CEval
>                          , CGreaterThan
>                          , CInteger
>                          , CLessThan
>                          , CModulus
>                          , CMultiply
>                          , CNegate
>                          , COr
>                          , COutput
>                          , CParen
>                          , CSubtract
>                          )
>                  , isInteger
>                  , isParen
>                  , str8ToString
>                  ) where

> import Data.Word ( Word8 )

Since all our strings will only contain 8-bit code points, let's create a type
alias:

> type String8 = [Word8]

We should also provide a (lossless) conversion function to a string of Chars.

> str8ToString :: String8 -> String
> str8ToString s8 = [toEnum $ fromEnum x | x <- s8]

The following values are prescribed by the specification:

\begin{itemize}
\item Integers, expressed as sequences of decimal digits.
\item Expressions in parentheses, which are considered strings. Such expressions
      may be nested.
\item Binary arithmetic operators for addition, subtraction, multiplication,
      division and modulus (positive remainder).
\item Binary logic operators: and, or.
\item Binary relational operators for equality, less-than and greater-than.
\item Unary arithmetic operators for integer negation.
\item The copy operator (also known as “pick”), which pops n off the stack and
      pushes the n-th element of the resulting stack.
\item The delete operator (also known as “drop nth”), which pops n off the stack
      and removes the n-th element of the resulting stack.
\item The eval operator, which pops a parenthesized expression, parses it as
      Cakeulator input, and places it on the stack to be evaluated next.
\item The char operator, which pops an integer n and a parenthesized expression
      (in either order) and pushes the ASCII code of the n-th character of the
      parenthesized expression.
\item The append operator, which pops an integer n and a parenthesized
      expression (in either order) and pushes a new parenthesized expression
      equal to the previous one with the character with ASCII value n appended.
\item The output operator, which first pops an integer p and then an integer c,
      and places the character with ASCII value c to position p.
\item The controlled input mode operator, which switches the parser into a mode
      where each line of input is pushed as a parenthesized expression onto the
      stack.
\item The normal input mode operator, which terminates controlled input mode.
\end{itemize}

(The last two are only relevant to the parser and therefore won't be placed onto
the stack.)

This makes the type definition rather long:

> data Value
>   = CInteger Integer
>   | CParen String8
>   | CAdd
>   | CSubtract
>   | CMultiply
>   | CDivide
>   | CModulus
>   | CAnd
>   | COr
>   | CEqual
>   | CLessThan
>   | CGreaterThan
>   | CNegate
>   | CCopy
>   | CDelete
>   | CEval
>   | CChar
>   | CAppend
>   | COutput
>   deriving (Eq, Show)

Let's add few type-identification functions:

> isInteger :: Value -> Bool
> isInteger (CInteger _) = True
> isInteger _            = False

> isParen :: Value -> Bool
> isParen (CParen _) = True
> isParen _          = False
