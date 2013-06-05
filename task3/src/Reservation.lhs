> module Reservation where

> import Route
> import Station
> import Train

> data Reservation = Reservation { train :: Train
>                                , route :: Route
>                                , from  :: Station
>                                , to    :: Station
>                                }
>       deriving (Show, Read)
