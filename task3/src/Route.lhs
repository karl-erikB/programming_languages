A route consists of several stations.

> module Route ( Route ( Route ) ) where

> import Station
> import Train

> data Route = Route { stations :: [ Station ]
>                    , trains :: [ Train ]
>                    }
>       deriving (Show, Read)
