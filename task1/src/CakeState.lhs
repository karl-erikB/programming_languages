This file defines a type for Cakeulator's state, consisting of the display and
the stack.

> module CakeState ( State ( State
>                          )
>                  , zeroState
>                  , display
>                  , stack
>                  , swizzleDisplay
>                  , swizzleStack
>                  ) where

We'll have to import the two relevant types:

> import qualified CakeDisplay as Di
> import qualified CakeStack as St
> import qualified CakeValue as Va

Now, we can define the type...

> data State = State Di.Display (St.Stack Va.Value)
>            deriving (Eq, Show)

... and a few data accessors.

> display :: State -> Di.Display
> display (State d _) = d

> stack :: State -> St.Stack Va.Value
> stack (State _ s) = s

Next, a constructor that creates the default state (clear display, empty stack).

> zeroState :: State
> zeroState = (State (Di.emptyDisplay) (St.Stack []))

Finally, let's add a few swizzlers, which replace one of the components.

> swizzleDisplay :: State -> Di.Display -> State
> swizzleDisplay (State _ s) d = State d s

> swizzleStack :: State -> St.Stack Va.Value -> State
> swizzleStack (State d _) s = State d s
