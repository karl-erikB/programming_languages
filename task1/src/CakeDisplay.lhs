This file implements Cakeulator's display. It consists of cells arranged in 4
rows by 64 columns; the cells are addressed in row-major order using the indices
0 to 255. Each cell contains an ASCII character (with a value of 0 to 255).

Internally, the characters are stored as bytes in an array.

> module CakeDisplay ( Display ( Display
>                              )
>                    , emptyDisplay
>                    , charAt
>                    , setCharAt
>                    , prettyFormat
>                    , prettyFormatBox
>                    ) where

Since arrays aren't part of the Prelude, we will want to import them here.

> import Data.Array ( Array
>                   , array
>                   , elems
>                   , (!)
>                   , (//)
>                   )

We're storing the characters and indices as 8-bit values, so let's import Word8
too.

> import Data.Word ( Word8
>                  )

Now, let's define a data type for the display, using Word8 as both the index
and element type.

> data Display = Display (Array Word8 Word8)
>              deriving (Show, Eq)

We will need a quick way to construct a display initialized to sane values
(specifically “space characters everywhere” as a representation of “empty
display”).

> emptyDisplay :: Display
> emptyDisplay = Display $ array (0, 255) [(i, 32) | i <- [0..255]]

Now, let's introduce a few operations on the display. The easiest are probably
getting and setting characters.

> charAt :: Display -> Word8 -> Word8
> charAt (Display a) i = a ! i

> setCharAt :: Display -> Word8 -> Word8 -> Display
> setCharAt (Display a) i c = Display (a // [(i, c)])

It would also be nice to have a possibility to pretty-format the display, i.e.
turn it into a string ready to be output. We will, however, need a helper
function for this, to add the newlines at the correct places.

> intercalateEach :: Integer -> [a] -> [a] -> [a]
> intercalateEach n ys xs
>   | n == 0    = error "n must not be 0."
>   | otherwise = intercalateNext xs ys n
>   where
>     intercalateNext []     ys i = []
>     intercalateNext xs     ys 0 = ys ++ (intercalateNext xs ys n)
>     intercalateNext (x:xs) ys i = x   : (intercalateNext xs ys (i-1))

We should also be able to convert the array into a string:

> displayToString :: Display -> String
> displayToString (Display a) = [(toEnum (fromIntegral e)) | e <- (elems a)]

The pretty-format function just adds newlines at opportune places:

> prettyFormat :: Display -> String
> prettyFormat d = intercalateEach 64 "\n" (displayToString d)

It would also be nice to show the borders of the display--Unicode allows for
this using the Box Drawing characters (U+2500 to U+257F).

> prettyFormatBox :: Display -> String
> prettyFormatBox d = top ++ middle ++ bottom
>   where
>     top    = "\x250C" ++ replicate 64 '\x2500' ++ "\x2510\n"
>     middle = "\x2502" ++ intercalateEach 64 "\x2502\n\x2502" (displayToString d) ++ "\x2502\n"
>     bottom = "\x2514" ++ replicate 64 '\x2500' ++ "\x2518"

And that's it!
