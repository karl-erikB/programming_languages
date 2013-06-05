> module Reservation where

> import Route
> import Station
> import Train

> data Reservation = Reservation { id    :: Int
>                                , train :: Train
>                                , route :: Route
>                                , from  :: Station
>                                , to    :: Station
>                                , seats :: [ Int ]
>                                }
>       deriving (Show, Read)
