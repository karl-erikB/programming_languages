A route consists of several stations.

> module Route ( Route ( Route )
>              , RouteSegment
>              , stations
>              , trains
>              , hubs
>              , routeByTrainAndWaypoints
>              , routeSegments
>              ) where

> import Data.Maybe
> import Data.List ( nub
>                  , intersect
>                  )

> import Station
> import Train

> data Route = Route { stations :: [ Station ]
>                    , trains :: [ Train ]
>                    }
>       deriving (Eq, Show, Read)

A route segment consists of the associated route, a source station
and a destination station.

> type RouteSegment = (Route, Station, Station)

Processes a list of routes and returns all hubs.
Hubs are stations which are present in more then one route.

> hubs :: [ Route ] -> [ Station ]
> hubs rs = nub $ concat $ hubs' rs
>   where hubs' (r:rs) = [ intersect (stations r) (stations r') | r' <- rs ] ++ hubs' rs
>         hubs' _      = []

> routeByTrainAndWaypoints :: [ Route ] -> Train -> Station -> Station -> Maybe Route
> routeByTrainAndWaypoints rs t src dst = listToMaybe $
>                                         routesByTrain t $
>                                         routesByStation src $
>                                         routesByStation dst rs
>   where routesByTrain t = filter (\(Route _ ts) -> t `elem` ts)

> routesByStation :: Station -> [ Route ] -> [ Route ]
> routesByStation s = filter (\(Route ss _) -> s `elem` ss)

Route segment determines all ways in which a destination can be reached from
a source. It returns a list containing all possible routings.

> routeSegments :: [ Route ] -> Station -> Station -> [[ RouteSegment ]]
> routeSegments routes src dst =
>           filter (not . null) $
>           map (compactSegments . reverse) $
>           concat [ routeSegments' routes [(r, src, src)] dst
>                  | r <- starts ]
>   where starts = routesByStation src routes

> routeSegments' :: [ Route ] -> [ RouteSegment ] -> Station -> [[ RouteSegment ]]
> routeSegments' routes ss@((route, prev, _):_) dst
>      | null remainingRoute = [[]]
>      | src == dst          = [(route, src, src):ss]
>      | otherwise           = concat [ routeSegments' routes ((r, src, src):ss) dst | r <- starts]
>   where remainingRoute = reverse $ takeWhile (/= prev) $ reverse $ stations route
>         src            = head remainingRoute
>         starts         = routesByStation src routes

Compacts consecutive RouteSegments of the same route into a single RouteSegment.

> compactSegments :: [ RouteSegment ] -> [ RouteSegment ]
> compactSegments ((r, src, dst):(r', src', dst'):cs)
>           | r == r'   = compactSegments $ (r, src, dst'):cs
>           | otherwise = (r, src, src') : compactSegments ((r', src', dst'):cs)
> compactSegments (s:[]) = [s]
> compactSegments _ = []
