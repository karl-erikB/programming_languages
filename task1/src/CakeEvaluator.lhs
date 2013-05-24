This module performs the actual evaluation of the operands on the stack.

> module CakeEvaluator ( evaluate
>                      , evaluateSingleStep
>                      , evaluateStepping
>                      , evaluateOne
>                      ) where

First, let's import all the pertinent modules.

> import Data.List ( genericIndex
>                  , genericLength
>                  , genericReplicate
>                  )
> import Data.Either.Unwrap ( fromLeft
>                           , fromRight
>                           , isLeft
>                           , isRight
>                           )
> import CakeStack ( Stack ( Stack
>                          )
>                  , depth
>                  , middropE
>                  , multipop
>                  , pickE
>                  , pop
>                  , popE
>                  , push
>                  )
> import CakeState ( State ( State
>                          )
>                  , display
>                  , stack
>                  , swizzleStack
>                  )
> import CakeDisplay ( Display ( Display
>                              )
>                    , setCharAt
>                    )
> import qualified CakeParser as Pa
> import qualified CakePrinter as Pr
> import qualified CakeValue as V

Now, let's define a function that evaluates the value at the top of the stack.
To simplify the patterns we will be matching, we will do this via a helper
function:

> evaluateOne :: V.Value -> Display -> (Stack V.Value) -> Either String (Display, (Stack V.Value))

The first argument is the value at the top of the stack, the second argument is
the display (from the state), the third argument is the rest of the stack, and
the function returns the display and stack.

Time to implement this function for all the value types.

Binary operators pop two elements and push the result. This helper function
abstracts away the stack operations. The display remains unchanged, so we just
shunt it through.

> evaluateIntBin :: (Integer -> Integer -> Either String Integer) -> Display -> (Stack V.Value) -> Either String (Display, Stack V.Value)
> evaluateIntBin f d stk
>   | (length operands) < 2    = Left "cannot apply binary operation: stack too shallow"
>   | not (V.isInteger lopval) = Left "cannot apply binary operation: left operand is not an integer"
>   | not (V.isInteger ropval) = Left "cannot apply binary operation: right operand is not an integer"
>   | isLeft reseither         = Left $ fromLeft reseither
>   | otherwise                = Right (d, push (V.CInteger $ fromRight reseither) stn)
>   where
>     (operands, stn)  = multipop 2 stk
>     lopval           = operands !! 0
>     ropval           = operands !! 1
>     (V.CInteger lop) = lopval
>     (V.CInteger rop) = ropval
>     reseither        = f lop rop

Many arithmetic operations are closed over integers. This wrapper reflects this
reality.

> closedIntBin :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Either a Integer
> closedIntBin f a b = Right (f a b)

Some aren't. We provide versions that handle this reality.

> eitherDiv :: Integer -> Integer -> Either String Integer
> eitherDiv a b
>   | b == 0    = Left "attempted division by zero"
>   | otherwise = Right (a `div` b)

> eitherMod :: Integer -> Integer -> Either String Integer
> eitherMod a b
>   | b == 0    = Left "attempted modulus with division by zero"
>   | otherwise = Right (a `mod` b)

The next function lets us pretend that even the relational operators are integer
binary operators.

> comparisonIntegers :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Either a Integer
> comparisonIntegers cmp a b = Right (if cmp a b then 0 else 1)

The next one does the same for and and or.

> binaryBools :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Either String Integer
> binaryBools cmp a b
>   | (a < 0) || (a > 1) = Left "boolean value of left operand out of range"
>   | (b < 0) || (b > 1) = Left "bolean value of right operand out of range"
>   | otherwise          = Right intgr
>   where
>     boola = intToBool a
>     boolb = intToBool b
>     boolr = cmp boola boolb
>     intgr = boolToInt boolr
>     -- 0 is true, 1 is false -- don't ask.
>     intToBool i = not $ toEnum $ fromIntegral i
>     boolToInt b = fromIntegral $ fromEnum $ not b

This function abstracts away stack details from operations that take an integral
value and a stack and return an error message or a new stack. The prescribed
language uses one-based addresses for the stack; Haskell uses zero-based
addresses for lists. The prescribed language counts the stack position including
the operands, the stack operations count without. These conversion are also
abstracted away here (by subtracting 2).

> intStackOp :: (Integer -> Stack V.Value -> Either String (Stack V.Value)) -> Display -> (Stack V.Value) -> Either String (Display, (Stack V.Value))
> intStackOp op d stk
>   | isLeft idxe            = Left $ fromLeft idxe
>   | not (V.isInteger idxv) = Left "cannot perform stack manipulation: n isn't an integer"
>   | isLeft zse             = Left $ fromLeft zse
>   | otherwise              = Right (d, fromRight zse)
>   where
>     idxe             = popE stk
>     (idxv, ys)       = fromRight idxe
>     (V.CInteger idx) = idxv
>     zse              = op (idx-2) ys

The following function abstracts away stack details from operations that take an
integer and a parenthesized value (in either order) and return a modified
parenthesized value.

> intParOp :: (Integer -> V.String8 -> Either String V.Value) -> Display -> (Stack V.Value) -> Either String (Display, (Stack V.Value))
> intParOp op d stk
>   | (length operands) < 2 = Left "cannot apply parenthesized expression operation: stack too shallow"
>   | lirp                  = packRes $ op loi rop
>   | rilp                  = packRes $ op roi lop
>   | otherwise             = Left "cannot apply parenthesized expression operation: one operand must be an integer, the other a parenthesized expression"
>   where
>     (operands, stn)   = multipop 2 stk
>     loval             = operands !! 0
>     roval             = operands !! 1
>     lirp              = (V.isInteger loval) && (V.isParen roval)
>     rilp              = (V.isParen loval) && (V.isInteger roval)
>     (V.CInteger loi)  = loval
>     (V.CParen lop)    = loval
>     (V.CInteger roi)  = roval
>     (V.CParen rop)    = roval
>     packRes (Left s)  = Left s
>     packRes (Right v) = Right (d, push v stn)

The functions relevant to the above are char and append:

> parChar :: Integer -> V.String8 -> Either String V.Value
> parChar i ins
>   | i < 0      = Left "cannot fetch character: index less than zero"
>   | (i >= len) = Right $ V.CInteger (-1)
>   | otherwise  = Right $ V.CInteger $ fromIntegral $ ins `genericIndex` i
>   where
>     len = genericLength ins

> parAppend :: Integer -> V.String8 -> Either String V.Value
> parAppend i ins = Right $ V.CParen $ ins ++ [c]
>   where
>     c = (fromIntegral i)

That's it for the helper functions -- time to define the actual evaluations.

Literal values---integers and parenthesized expressions---remain unmodified.

> evaluateOne v@(V.CInteger _a) d s = Right (d, push v s)
> evaluateOne v@(V.CParen   _p) d s = Right (d, push v s)

Now let's define the binary operators.

> evaluateOne V.CAdd         d s = evaluateIntBin (closedIntBin (+)) d s
> evaluateOne V.CSubtract    d s = evaluateIntBin (closedIntBin (-)) d s
> evaluateOne V.CMultiply    d s = evaluateIntBin (closedIntBin (*)) d s
> evaluateOne V.CDivide      d s = evaluateIntBin (eitherDiv)        d s
> evaluateOne V.CModulus     d s = evaluateIntBin (eitherMod)        d s

> evaluateOne V.CAnd         d s = evaluateIntBin (binaryBools (&&)) d s
> evaluateOne V.COr          d s = evaluateIntBin (binaryBools (||)) d s

> evaluateOne V.CLessThan    d s = evaluateIntBin (comparisonIntegers (<))  d s
> evaluateOne V.CGreaterThan d s = evaluateIntBin (comparisonIntegers (>))  d s

Equality must work on parenthesized expressions as well, so it's a special case.

> evaluateOne V.CEqual d stk
>   | (length operands) < 2 = Left "cannot compare: stack too shallow"
>   | lopinvalid            = Left "cannot compare: left operand is not a value"
>   | ropinvalid            = Left "cannot compare: right operand is not a value"
>   | otherwise             = Right (d, push (V.CInteger ret) stn)
>   where
>     (operands, stn)  = multipop 2 stk
>     lopval           = operands !! 0
>     ropval           = operands !! 1
>     lopinvalid       = not (V.isInteger lopval) && not (V.isParen lopval)
>     ropinvalid       = not (V.isInteger ropval) && not (V.isParen ropval)
>     ret              = if lopval == ropval then 0 else 1

Negation is relatively simple.

> evaluateOne V.CNegate d stk
>   | len < 1               = Left "cannot negate: stack too shallow"
>   | not (V.isInteger val) = Left "cannot negate: value not an integer"
>   | otherwise             = Right (d, Stack $ (V.CInteger (-ival)):ys)
>   where
>     len               = depth stk
>     (Stack xs)        = stk
>     (val:ys)          = xs
>     (V.CInteger ival) = val

Copy and delete perform the respective stack operations.

> evaluateOne V.CCopy   d s = intStackOp (pickE)    d s
> evaluateOne V.CDelete d s = intStackOp (middropE) d s

Evaluate parses the parenthetized expression and pushes the stack onto the
current stack. If the top of the stack is not a parenthetized expression, leave
it unchanged.

> evaluateOne V.CEval d stk
>   | len < 1             = Left "cannot evaluate: stack too shallow"
>   | not (V.isParen val) = Right (d, stk)
>   | isLeft parseres     = Left ("cannot evaluate: parsing error: " ++ fromLeft parseres)
>   | otherwise           = Right (d, Stack zs)
>   where
>     len             = depth stk
>     (Stack xs)      = stk
>     (val:ys)        = xs
>     (V.CParen sval) = val
>     parseres        = Pa.parse $ V.str8ToString sval
>     (Stack ps)      = fromRight parseres
>     zs              = ps ++ ys

Char and append perform the respective integer-and-parenthesized-expression
operations.

> evaluateOne V.CChar   d s = intParOp (parChar)   d s
> evaluateOne V.CAppend d s = intParOp (parAppend) d s

Output is the only one that changes the display.

> evaluateOne V.COutput d stk
>   | len < 2                  = Left "cannot output: stack too shallow"
>   | not (V.isInteger posval) = Left "cannot output: position not an integer"
>   | not (V.isInteger chrval) = Left "cannot output: character not an integer"
>   | posOOR                   = Left "cannot output: position out of range"
>   | chrOOR                   = Left "cannot output: character out of range"
>   | otherwise                = Right (newdisp, stn)
>   where
>     (ops, stn)          = multipop 2 stk
>     len                 = genericLength ops
>     posval              = ops !! 0
>     chrval              = ops !! 1
>     (V.CInteger posint) = posval
>     (V.CInteger chrint) = chrval
>     posOOR              = (posint < 0) || (posint > 255)
>     chrOOR              = (chrint < 0) || (chrint > 255)
>     pos                 = fromIntegral posint
>     chr                 = fromIntegral chrint
>     newdisp             = setCharAt d pos chr

Now that this load of code is finally behind us, define the function that takes
a stack and evaluates it.

The function starts at the deepest point of the stack and works its way upward.
Each value is evaluated (with access to the tail of the stack) repeatedly until
the stack stops changing. Then, processing moves one level upward (this
effectively means the next value is pushed onto the stack) and the same
evaluation shebang happens again. If an error (Left) is encountered, it
immediately percolates upward.

Due to the support of the eval operator, anytime the stack changes, processing
must start anew at the bottom.

Defining a single-step evaluation function first allows some step-by-step
coolness later on.

> evaluateSingleStep :: State -> Either String State
> evaluateSingleStep st8@(State dsp stk)
>   | (depth stk) == 0 = Right st8
>   | isLeft subres    = Left $ fromLeft subres
>   | substk /= srstk  = Right $ State srdsp (push tos srstk)
>   | isLeft meres     = Left $ fromLeft meres
>   | otherwise        = Right $ State medsp mestk
>   where
>     (tos, substk)       = pop stk
>     subres              = evaluateSingleStep (State dsp substk)
>     (State srdsp srstk) = fromRight subres
>     meres               = evaluateOne tos srdsp srstk
>     (medsp, mestk)      = fromRight meres

> evaluate :: State -> Either String State
> evaluate st8@(State dsp stk)
>    | isLeft oneres = oneres
>    | stk == newstk = oneres
>    | otherwise     = evaluate onest8
>    where
>      oneres           = evaluateSingleStep st8
>      onest8           = fromRight oneres
>      (State _ newstk) = onest8

... and this is the promised step-by-step coolness.

> evaluateStepping :: State -> IO (Either String State)
> evaluateStepping st8@(State dsp stk)
>   | isLeft oneres = return oneres
>   | stk == newstk = return oneres
>   | otherwise     = do
>     putStrLn $ Pr.prettyStack stk
>     evaluateStepping onest8
>   where
>     oneres           = evaluateSingleStep st8
>     onest8           = fromRight oneres
>     (State _ newstk) = onest8
