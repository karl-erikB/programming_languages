This module represents a train station and defines a couple of
useful functions.

> module Station ( Station ( Station )
>                , stationByName
>                ) where

> import Data.Maybe

A station consists of a code and its name.

> data Station = Station { name :: String } 
>       deriving (Eq, Show, Read)

> stationByName :: [ Station ] -> String -> Maybe Station
> stationByName ss n = listToMaybe $ filter (\s -> name s == n) ss
