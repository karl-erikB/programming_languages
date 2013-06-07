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

At this point, we can be sure that the requested route exists and we only need to check
whether enough space is left to complete the reservation. rs only contains Reservations applicable
to the current train.

> reserveSeats :: (Route, Train, Station, Station) -> [ Reservation ] -> Integer -> Maybe Reservation
> reserveSeats (r, t, src, dst) rs n
>       | minFreeSeatsExceeded = Nothing

TODO: Fetch new id, check if seats are possible (seats start at 0 in the first wagon,
and continue sequentially throughout the entire train), and determine reserved seats.

>       | otherwise = Just $ Reservation 0 t r src dst [1,2,3,4,5]
>   where minFreeSeatsExceeded = ((trainCapacity t) - filledSeats) < (minimumFreeSeats t)
>         filledSeats          = foldr (\res sum -> sum + (fromIntegral $ length $ seats res)) 0 rs
