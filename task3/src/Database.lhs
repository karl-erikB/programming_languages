> module Database ( Database ( Database )
>                 , printDatabase
>                 , printMultiSegment
>                 , printReservations
>                 , printSegment
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
>         rs     = Re.reservationsByTrainAndSeats allRs train' [seat]

> printReservations' :: [ Re.Reservation ] -> String
> printReservations' (r:rs) = Re.printReservation r ++ "\n" ++ printReservations' rs
> printReservations' _      = ""

Given a source and destination station, prints the minimum count of available
seats and maximum count of reserved seats between any two consecutive
stations within this segment.

> printSegment :: Database -> [ String ] -> String
> printSegment db args
>       | length args /= 3 = "Invalid argument count"
>       | isNothing src    = "Source station not found"
>       | isNothing dst    = "Destination station not found"
>       | otherwise        = printSegment' rs routes src' dst'
>   where stations = allStations db
>         src      = stationByName stations $ args !! 1
>         src'     = fromJust src
>         dst      = stationByName stations $ args !! 2
>         dst'     = fromJust dst
>         routes   = routesWithSegment db src' dst'
>         rs       = reservations db

> printSegment' :: [ Re.Reservation ] -> [ Route ] -> Station -> Station -> String
> printSegment' res (ro:ros) src dst = printRouteStats res ro src dst ++ "\n" ++
>                                      printSegment' res ros src dst
> printSegment' _ [] _ _ = ""

> printRouteStats res r@(Route st (t:ts)) src dst
>       | length segment < 2 = "Segment must include > 1 stations"
>       | otherwise          = printRouteStats' res (r, t) segment 0 ++ "\n" ++
>                              printRouteStats res (Route st ts) src dst
>   where segment = routeSegment r src dst
> printRouteStats _ _ _ _ = ""

Finally we're down to the wagon level. Collect stats for each wagon
and print them.

> printRouteStats' :: [ Re.Reservation ] -> (Route, Train) -> [ Station ] -> Int -> String
> printRouteStats' res (ro, t@(Train nm (w:ws) mfs)) seg i =
>           nm ++ ": min free " ++ show min' ++
>           ", max reserved " ++ show max' ++
>           " at " ++ show src' ++ " - " ++ show dst' ++ "\n" ++
>           printRouteStats' res (ro, Train nm ws mfs) seg j
>   where j                  = i + (fromInteger $ seats w)
>         (src', dst', max') = segmentExtrema res (ro, t) seg (i, j - 1)
>         min'               = (fromInteger $ seats w) - max'
> printRouteStats' _ _ _ _ = ""

Looks up the extrema (minimum free seats, maximum reserved seats)
over all stations within the segment for seats ranging from [i, j].

> segmentExtrema :: [ Re.Reservation ] -> (Route, Train) -> [ Station ]
>                -> (Int, Int) -> (Station, Station, Int)
> segmentExtrema res (ro, t) (s1:s2:ss) (i, j)
>       | taken' > taken = (s1', s2', taken')
>       | otherwise      = (s1, s2, taken)
>   where res'   = Re.reservationsByTrainAndSeats res t [i .. j - 1]
>         res''  = Re.reservationsForTrainAndSegment res' (ro, t, s1, s2)
>         counts = map (\(Re.Reservation _ _ _ _ _ ss) -> length ss) res''
>         taken  = sum counts
>         (s1', s2', taken') = segmentExtrema res (ro, t) (s2:ss) (i, j)

> segmentExtrema _ _ _ _ = (Station "Null", Station "Null", 0)

> routesWithSegment :: Database -> Station -> Station -> [ Route ]
> routesWithSegment db src dst = filter containsSegment $ routes db
>   where containsSegment (Route ss _) = let ss' = dropWhile (/= src) ss
>                                        in src `elem` ss && dst `elem` ss'

This function is somewhat similar to printSegment. However, it does not group
its results by train and wagon, and it can handle a segment which includes
more than one route.

First, we need to determine in which ways we can actually reach dst from src.

> printMultiSegment :: Database -> [ String ] -> String
> printMultiSegment db args
>       | length args /= 3 = "Invalid argument count"
>       | isNothing src    = "Source station not found"
>       | isNothing dst    = "Destination station not found"
>       | otherwise        = printMultiSegment' rs segments
>   where stations = allStations db
>         src      = stationByName stations $ args !! 1
>         src'     = fromJust src
>         dst      = stationByName stations $ args !! 2
>         dst'     = fromJust dst
>         segments = routeSegments (routes db) src' dst'
>         rs       = reservations db

> printMultiSegment' :: [ Re.Reservation ] ->[[ RouteSegment ]] -> String
> printMultiSegment' es (rs:rss) = printMultiSegment'' es rs ++ "\n" ++
>                                  printMultiSegment' es rss
> printMultiSegment' _ _         = ""

> printMultiSegment'' :: [ Re.Reservation ] -> [ RouteSegment ] -> String
> printMultiSegment'' es ((route, Station src, Station dst):rss) =
>               src ++ " - " ++ dst ++ ": " ++ show min' ++ " min free, " ++
>               show max' ++ " max group size on train '" ++ nm ++ "'\n" ++
>               printMultiSegment'' es rss
>   where (Train nm _ _, min', max') = aggregateSegment es route (Station src) (Station dst)
> printMultiSegment'' _ _ = ""

> aggregateSegment :: [ Re.Reservation ] -> Route -> Station -> Station ->
>                     (Train, Int, Int)
> aggregateSegment res ro src dst = undefined
>   where segment = routeSegment ro src dst
>         seats t = (0, fromInteger $ trainCapacity t - 1)
>         byTrain = [ (t, segmentExtrema res (ro, t) segment (seats t))
>                   | t <- trains ro
>                   ]

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
