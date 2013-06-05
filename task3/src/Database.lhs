> module Database where

> import Route
> import Reservation
> import Station

> data Database = Database { route        :: [ Route ]
>                          , reservations :: [ Reservation ]
>                          }
>       deriving (Show, Read)
