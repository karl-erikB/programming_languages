> module Train ( Train ( Train )
>              , Wagon ( Wagon )
>              , minimumFreeSeats
>              , seats
>              , trainByName
>              , trainCapacity
>              , wagons
>              , wagonSeat
>              ) where

> import Data.Maybe

> data Wagon = Wagon { seats :: Integer
>                    }
>       deriving (Show, Read, Eq)

> data Train = Train { name :: String
>                    , wagons :: [ Wagon ]
>                    , minimumFreeSeats :: Integer
>                    }
>       deriving (Show, Read, Eq)

> trainByName :: [ Train ] -> String -> Maybe Train
> trainByName ts n = listToMaybe $ filter (\t -> name t == n) ts

> trainCapacity :: Train -> Integer
> trainCapacity t = sum [ seats w | w <- wagons t ]

> wagonSeat :: Train -> Wagon -> Int
> wagonSeat (Train _ ws _) w = wagonSeat' ws w 0
>   where wagonSeat' :: [ Wagon ] -> Wagon -> Int -> Int
>         wagonSeat' (w:ws) w' i = if w == w' then i else wagonSeat' ws w' (i + fromInteger (seats w))
>         wagonSeat' _ _ _       = -1
