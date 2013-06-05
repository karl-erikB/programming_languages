A route consists of several stations.

> module Route ( Route ( Route )
>              , stations
>              , trains
>              , hubs
>              ) where

> import Data.List ( nub
>                  , intersect
>                  )

> import Station
> import Train

> data Route = Route { stations :: [ Station ]
>                    , trains :: [ Train ]
>                    }
>       deriving (Show, Read)

Processes a list of routes and returns all hubs.
Hubs are stations which are present in more then one route.

> hubs :: [ Route ] -> [ Station ]
> hubs rs = nub $ concat $ hubs' rs
>   where hubs' (r:rs) = [ intersect (stations r) (stations r') | r' <- rs ] ++ hubs' rs
>         hubs' _      = []

