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
> routeByTrainAndWaypoints rs t src dst = listToMaybe $ filter routeMatches rs
>   where routeMatches r = t `elem` (trains r) &&
>                          src `elem` (stations r) &&
>                          dst `elem` (stations r) 

Route segment determines all ways in which a destination can be reached from
a source. It returns a list containing all possible routings.

> routeSegments :: [ Route ] -> Station -> Station -> [ [ RouteSegment ] ]
> routeSegments routes src dst = undefined
