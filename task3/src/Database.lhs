> module Database ( Database ( Database )
>                 , printDatabase
>                 , printReservations
>                 , addReservation
>                 , delReservation
>                 ) where

> import Data.List ( (\\)
>                  , nub
>                  )
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

Displays all reservation for the given seat in the given train.

> printReservations :: Database -> [ String ] -> String
> printReservations db args
>       | length args /= 3   = "Invalid argument count"
>       | notInt (args !! 2) = "Could not parse integer"
>       | isNothing train    = "Train not found"
>       | otherwise          = printReservations' rs
>   where seat  :: Int
>         seat   = read $ args !! 2
>         trains = allTrains db
>         train  = trainByName trains $ args !! 1
>         train' = fromJust train
>         allRs  = reservations db
>         rs     = Re.reservationsByTrainAndSeat allRs train' seat

> printReservations' :: [ Re.Reservation ] -> String
> printReservations' (r:rs) = Re.printReservation r ++ "\n" ++ printReservations' rs
> printReservations' _      = ""

Deletes a reservation if it exists.

> delReservation :: Database -> [ String ] -> (String, Database)
> delReservation db args
>       | length args /= 2   = ("Invalid argument count", db)
>       | notInt (args !! 1) = ("Could not parse integer", db)
>       | otherwise          = ("Reservation deleted", delReservation' db rs')
>   where i :: Integer
>         i = read $ args !! 1
>         rs = reservations db
>         rs' = Re.reservationsById rs i

> delReservation' :: Database -> [ Re.Reservation ] -> Database
> delReservation' (Database ro re) rs = Database ro (re \\ rs)

Adds a reservation (if possible).
Args contains the train name, source and destination stations, and the number
of requested seats. Performs initial lookups and parsing of incoming string
parameters.

> addReservation :: Database -> [ String ] -> (String, Database)
> addReservation db args
>       | length args /= 5   = ("Invalid argument count", db)
>       | notInt (args !! 4) = ("Could not parse integer", db)
>       | count <= 0         = ("Count must be strictly positive", db)
>       | isNothing train    = ("Train not found", db)
>       | isNothing src      = ("Source station not found", db)
>       | isNothing dst      = ("Destination station not found", db)
>       | isNothing route    = ("Route not found", db)
>       | isNothing r        = ("Reservation is not possible", db)
>       | otherwise          = ("Reservation placed successfully", appendReservation db r')
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

> notInt :: String -> Bool
> notInt = not . all (\c -> '0' <= c && c <= '9')
