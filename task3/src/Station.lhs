This module represents a train station and defines a couple of
useful functions.

> module Station ( Station ( Station
>                          )
>                ) where

A station consists of a code and its name.

> data Station = Station { name :: String } 
>       deriving (Eq, Show, Read)
