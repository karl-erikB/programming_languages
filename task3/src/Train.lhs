> module Train ( Train ( Train )
>              , Wagon ( Wagon )
>              , minimumFreeSeats
>              , trainByName
>              , trainCapacity
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
> trainCapacity t = trainCapacity' $ wagons t
>   where trainCapacity' = foldr (\w sum -> sum + seats w) 0
