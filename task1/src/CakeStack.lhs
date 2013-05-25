This file implements Cakeulator's stack.

> module CakeStack  ( Stack  ( Stack
>                            )
>                   , depth
>                   , middropE
>                   , multipeek
>                   , multipop
>                   , multipush
>                   , peek
>                   , peekE
>                   , pickE
>                   , pop
>                   , popE
>                   , push
>                   ) where

As always, the necessary imports come first.

> import Data.Either  ( Either
>                     , either
>                     )
> import Data.List  ( genericLength
>                   , genericSplitAt
>                   , genericTake
>                   )

The stack is simply a list of elements.

> data Stack a = Stack [a]

We implement the Eq and Show typeclasses because there is no good reason not to:

> instance (Eq a) => Eq (Stack a) where
>   (Stack as) == (Stack bs) = (as == bs)

> instance (Show a) => Show (Stack a) where
>   show (Stack as) = "Stack " ++ (show as)

\textit{depth} simply returns the length of the underlying list.

> depth :: (Integral n) => Stack a -> n
> depth (Stack xs) = genericLength xs

\textit{push} is simple---just add the element to the front.

> push :: a -> Stack a -> Stack a
> push a (Stack bs) = Stack (a:bs)

pop and peek are a bit more complex, since we have to deal with empty stacks.
Let's implement them using Either (for soft error handling) first.

> popE :: Stack a -> Either String (a, Stack a)
> popE (Stack [])      = Left "Popping from an empty stack."
> popE (Stack (a:as))  = Right (a, Stack as)
>
> peekE :: Stack a -> Either String a
> peekE (Stack [])     = Left "Peeking at an empty stack."
> peekE (Stack (a:_))  = Right a

Using the soft-error versions, we can implement hard-error versions.

> pop :: Stack a -> (a, Stack a)
> pop stk = either (\str -> error str) id $ popE stk
>
> peek :: Stack a -> a
> peek stk = either (\str -> error str) id $ peekE stk

Let's also implement functions to push, peek at, or pop multiple elements. The
last two will simply return a shorter list if the stack is too shallow.

> multipush :: [a] -> Stack a -> Stack a
> multipush xs (Stack ys) = (Stack $ xs ++ ys)
>
> multipop :: (Integral n) => n -> Stack a -> ([a], Stack a)
> multipop i (Stack xs) = (ls, Stack rs)
>   where
>     (ls, rs) = genericSplitAt i xs
>
> multipeek :: (Integral n) => n -> Stack a -> [a]
> multipeek i (Stack xs) = genericTake i xs

Finally, a few middle-of-the-stack operations. \textit{middrop} deletes a value
in the middle of the stack.

> middropE :: (Integral n) => n -> Stack a -> Either String (Stack a)
> middropE i (Stack xs)
>   | i < 0      = Left "Negative stack index."
>   | i >= len   = Left "Index greater than stack depth."
>   | otherwise  = Right (Stack (ls ++ qs))
>   where
>     len       = genericLength xs
>     (ls, rs)  = genericSplitAt i xs
>     q:qs      = rs

\textit{pick} chooses a value from the middle of the stack and copies it to the
top.

> pickE :: (Integral n) => n -> Stack a -> Either String (Stack a)
> pickE i (Stack xs)
>   | i < 0     = Left "Negative stack index."
>   | i >= len  = Left "Index greater than stack depth."
>   | otherwise = Right (Stack (q:(ls ++ rs)))
>   where
>     len      = genericLength xs
>     (ls, rs) = genericSplitAt i xs
>     q:_      = rs

And that's it for the stack!
