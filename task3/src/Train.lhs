> module Train ( Train ( Train )
>              , Wagon ( Wagon )
>              ) where

> data Wagon = Wagon { seats :: Integer
>                    }
>       deriving (Show, Read)

> data Train = Train { wagons :: [ Wagon ]
>                    , minimumFreeSeats :: Integer
>                    }
>       deriving (Show, Read)
