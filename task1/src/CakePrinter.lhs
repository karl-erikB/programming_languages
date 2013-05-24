The opposite of the parser, this module pretty-prints Cakeulator values.

> module CakePrinter ( prettyStack
>                    , prettyValue
>                    ) where

This module imports more than it exports. One could say it suffers from a trade
deficit.

> import qualified CakeValue as V
> import CakeStack ( Stack ( Stack
>                          )
>                  )

This function prettifies a single value.

> prettyValue :: V.Value -> String

Integers are output verbatim.

> prettyValue (V.CInteger i) = (show i)

Parenthesized expressions are put back in their parentheses and otherwise
produced verbatim. A space follows them because it looks nicer.

> prettyValue (V.CParen s) = "(" ++ V.str8ToString s ++ ")"

Operators are spat out as their symbols, again followed by a space.

> prettyValue V.CAdd = "+"
> prettyValue V.CSubtract = "-"
> prettyValue V.CMultiply = "*"
> prettyValue V.CDivide = "/"
> prettyValue V.CModulus = "%"
> prettyValue V.CAnd = "&"
> prettyValue V.COr = "|"
> prettyValue V.CEqual = "="
> prettyValue V.CLessThan = "<"
> prettyValue V.CGreaterThan = ">"
> prettyValue V.CNegate = "~"
> prettyValue V.CCopy = "!"
> prettyValue V.CDelete = "#"
> prettyValue V.CEval = "@"
> prettyValue V.CChar = "?"
> prettyValue V.CAppend = ";"
> prettyValue V.COutput = "$"

This function prettifies a whole stack, as if it had been typed into Cakeulator.
Each token is followed by a space for increased beauty.

> prettyStack :: Stack V.Value -> String
> prettyStack (Stack vs) = prettyRecurse "" vs
>   where
>     prettyRecurse str []     = str
>     prettyRecurse str (v:vs) = prettyRecurse (prettyValue v ++ (' ':str)) vs
