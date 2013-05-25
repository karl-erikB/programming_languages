This module defines a tri-state type useful for controlling data-processing
recursions.

> module RepeatState  ( RepeatState  ( Failure
>                                    , Repeat
>                                    , Stop
>                                    )
>                     , fromFailure
>                     , fromRepeat
>                     , fromStop
>                     , isFailure
>                     , isRepeat
>                     , isStop
>                     ) where

\textit{RepeatState} has three constructors: \textit{Repeat} (to perform another
call), \textit{Stop} (to stop calling and continue with the next step), and
\textit{Failure} (to stop calling and abort).

> data RepeatState r s f = Repeat r | Stop s | Failure f

\textit{Eq} and \textit{Show} are, once again, good ideas:

> instance (Eq r, Eq s, Eq f) => Eq (RepeatState r s f) where
>   (Repeat   a)  == (Repeat   b)  = (a == b)
>   (Stop     a)  == (Stop     b)  = (a == b)
>   (Failure  a)  == (Failure  b)  = (a == b)
>   _             == _             = False
>
> instance (Show r, Show s, Show f) => Show (RepeatState r s f) where
>   show (Repeat   a)  = show $ "Repeat "   ++ show a
>   show (Stop     a)  = show $ "Stop "     ++ show a
>   show (Failure  a)  = show $ "Failure "  ++ show a

The following functions help test the type of state:

> isRepeat   :: RepeatState r s f -> Bool
> isRepeat    (Repeat _)   = True
> isRepeat    _            = False
>
> isStop     :: RepeatState r s f -> Bool
> isStop      (Stop _)     = True
> isStop      _            = False
>
> isFailure  :: RepeatState r s f -> Bool
> isFailure   (Failure _)  = True
> isFailure   _            = False

The following functions unwrap the state:

> fromRepeat   :: RepeatState r s f -> r
> fromRepeat    (Repeat r)   = r
> fromRepeat    _            = error "unwrapping non-Repeat RepeatState"
>
> fromStop     :: RepeatState r s f -> s
> fromStop      (Stop s)     = s
> fromStop      _            = error "unwrapping non-Stop RepeatState"
>
> fromFailure  :: RepeatState r s f -> f
> fromFailure   (Failure f)  = f
> fromFailure   _            = error "unwrapping non-Failure RepeatState"
