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

> reservationsForTrain :: [ Reservation ] -> Train -> [ Reservation ]
> reservationsForTrain rs t = filter (\r -> train r == t) rs

> reserveSeats :: Train -> [ Reservation ] -> Integer -> Maybe Reservation
> reserveSeats t rs n = undefined
