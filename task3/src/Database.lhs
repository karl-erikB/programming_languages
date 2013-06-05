> module Database ( Database ( Database )
>                 , printDatabase
>                 ) where

> import Route
> import Reservation
> import Station
> import Train

> data Database = Database { route        :: [ Route ]
>                          , reservations :: [ Reservation ]
>                          }
>       deriving (Show, Read)

> printDatabase :: Database -> String
> printDatabase (Database routes _) =
>       "Routes:\n" ++ printRoutes routes ++
>       "Hubs:\n\n" ++ printStations (hubs routes)

> printRoutes :: [ Route ] -> String
> printRoutes (r:rs) = printRoute r ++ "\n" ++ printRoutes rs
> printRoutes _      = ""

> printRoute :: Route -> String
> printRoute r = "Route:\n\n" ++ printStations (stations r) ++ "\n"
>                             ++ printTrains (trains r) ++ "\n"

> printStations :: [ Station ] -> String
> printStations (s:ss) = show s ++ "\n" ++ printStations ss
> printStations _      = ""

> printTrains :: [ Train ] -> String
> printTrains (t:ts) = show t ++ "\n" ++ printTrains ts
> printTrains _      = ""
