> module Database ( Database ( Database )
>                 , printDatabase
>                 , addReservation
>                 ) where

> import Data.List ( nub )
> import Data.Maybe

> import Route
> import qualified Reservation as Re
> import Station
> import Train

> data Database = Database { route        :: [ Route ]
>                          , reservations :: [ Re.Reservation ]
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

Adds a reservation (if possible).
Args contains the train name, source and destination stations, and the number
of requested seats.

> addReservation :: Database -> [ String ] -> (String, Database)
> addReservation db args = if valid then undefined else ("Invalid argument", db)
>   where count    :: Integer
>         count    = read $ args !! 4
>         trains   = allTrains db
>         stations = allStations db
>         train    = trainByName trains $ args !! 1
>         from     = stationByName stations $ args !! 2
>         to       = stationByName stations $ args !! 3
>         valid    = all isJust [ from, to ] && isJust train

> allTrains :: Database -> [ Train ]
> allTrains = concat . (map trains) . route

> allStations :: Database -> [ Station ]
> allStations = nub . concat . (map stations) . route
