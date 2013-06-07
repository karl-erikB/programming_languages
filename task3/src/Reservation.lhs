> module Reservation ( Reservation ( Reservation )
>                    , reserveSeats
>                    ) where

> import Route
> import Station
> import Train

> data Reservation = Reservation { resid :: Int
>                                , train :: Train
>                                , route :: Route
>                                , from  :: Station
>                                , to    :: Station
>                                , seats :: [ Int ]
>                                }
>       deriving (Show, Read)

Retrieves all reservations applicable to the given route segment on the given train.

> reservationsForTrainAndSegment :: [ Reservation ] -> (Route, Train, Station, Station) -> [ Reservation ]
> reservationsForTrainAndSegment rs (r, t, src, dst) = filter (\r -> train r == t) rs

At this point, we can be sure that the requested route exists and we only need to check
whether enough space is left to complete the reservation.

> reserveSeats :: [ Reservation ] -> (Route, Train, Station, Station) -> Integer -> Maybe Reservation
> reserveSeats rs (r, t, src, dst) n
>       | minFreeSeatsExceeded = Nothing
>       | null bookedSeats     = Nothing
>       | otherwise            = Just $ Reservation newId t r src dst bookedSeats
>   where newId                = 1 + maximum [ resid res  | res <- rs ]
>         applicableRs         = reservationsForTrainAndSegment rs (r, t, src, dst)
>         filledSeats          = fromIntegral $ sum [ length $ seats res | res <- applicableRs ]
>         minFreeSeatsExceeded = ((trainCapacity t) - filledSeats) < (minimumFreeSeats t)
>         bookedSeats          = undefined
