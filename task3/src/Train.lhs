> module Train ( Train ( Train )
>              , Wagon ( Wagon )
>              ) where

> data Wagon = Wagon { seats :: Integer
>                    }
>       deriving (Show, Read)

> data Train = Train { name :: String
>                    , wagons :: [ Wagon ]
>                    , minimumFreeSeats :: Integer
>                    }
>       deriving (Show, Read)
