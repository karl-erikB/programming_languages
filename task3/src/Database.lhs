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

> data Database = Database { routes       :: [ Route ]
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
of requested seats. Performs initial lookups and parsing of incoming string
parameters.

> addReservation :: Database -> [ String ] -> (String, Database)
> addReservation db args
>       | count <= 0      = ("Count must be strictly positive", db)
>       | isNothing train = ("Train not found", db)
>       | isNothing src   = ("Source station not found", db)
>       | isNothing dst   = ("Destination station not found", db)
>       | isNothing route = ("Route not found", db)
>       | isNothing r     = ("Reservation is not possible", db)
>       | otherwise       = ("Reservation placed successfully", appendReservation db r')
>   where count    :: Integer
>         count    = read $ args !! 4
>         trains   = allTrains db
>         stations = allStations db
>         train    = trainByName trains $ args !! 1
>         train'   = fromJust train
>         src      = stationByName stations $ args !! 2
>         src'     = fromJust src
>         dst      = stationByName stations $ args !! 3
>         dst'     = fromJust dst
>         route    = routeByTrainAndWaypoints (routes db) train' src' dst'
>         route'   = fromJust route
>         r        = Re.reserveSeats (reservations db) (route', train', src', dst') count
>         r'       = fromJust r

> appendReservation :: Database -> Re.Reservation -> Database
> appendReservation (Database routes' reservations') r = Database routes' (r:reservations')

> allTrains :: Database -> [ Train ]
> allTrains = concat . (map trains) . routes

> allStations :: Database -> [ Station ]
> allStations = nub . concat . (map stations) . routes
