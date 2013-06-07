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
> addReservation db args = addReservation' db train from to count
>   where count    :: Integer
>         count    = read $ args !! 4
>         trains   = allTrains db
>         stations = allStations db
>         train    = trainByName trains $ args !! 1
>         from     = stationByName stations $ args !! 2
>         to       = stationByName stations $ args !! 3

> addReservation' :: Database -> Maybe Train -> Maybe Station -> Maybe Station
>                             -> Integer -> (String, Database)
> addReservation' db (Just t) (Just src) (Just dst) cnt
>   | cnt <= 0    = ("Count must be strictly positive", db)
>   | isNothing route = ("Route not found", db)
>   | isNothing r = ("Reservation is not possible", db)
>   | otherwise   = ("Reservation placed successfully", dirtyDb)
>   where route   = routeByTrainAndWaypoints (routes db) t src dst
>         reserv  = Re.reservationsForTrain (reservations db) t
>         r       = Re.reserveSeats (fromJust route, t, src, dst) reserv cnt
>         dirtyDb = Database (routes db) ((fromJust r):(reservations db))

If any of the incoming arguments are Nothing, return an error message.

> addReservation' db _ _ _ _ = ("Invalid argument", db)

> allTrains :: Database -> [ Train ]
> allTrains = concat . (map trains) . routes

> allStations :: Database -> [ Station ]
> allStations = nub . concat . (map stations) . routes
