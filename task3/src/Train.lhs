> module Train ( Train ( Train )
>              , Wagon ( Wagon )
>              , trainByName
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
