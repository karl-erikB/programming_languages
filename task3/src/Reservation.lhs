> module Reservation ( Reservation ( Reservation )
>                    , reserveSeats
>                    ) where

> import Data.List ( intersect )

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
Note that applicable in this case is defined as including the station.

> reservationsForTrainAndSegment :: [ Reservation ] -> (Route, Train, Station, Station) -> [ Reservation ]
> reservationsForTrainAndSegment rs (r, t, src, dst) = bySegment

First, discard all reservations that aren't made for this train.

>   where byTrain    = filter (\r -> train r == t) rs

A segment includes all stations within [Src, Dst] in this route. Discard all remaining
reservations that do not overlap with the current segment.

>         ownSegment = segment src dst (stations r)
>         bySegment  = filter (\(Reservation _ _ _ src' dst' _) -> not $ null $
>                           intersect ownSegment $ segment src' dst' (stations r)) byTrain

Given a list containing a followed by b, returns a list containing all elements
[a, ..., b].

> segment :: (Eq a) => a -> a -> [ a ] -> [ a ]
> segment fst lst = reverse . dropWhile (/= lst) . reverse . dropWhile (/= fst)

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
