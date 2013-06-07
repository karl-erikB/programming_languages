> module Reservation ( Reservation ( Reservation )
>                    , reserveSeats
>                    ) where

> import Data.List ( intersect
>                  , (\\)
>                  )

> import Route
> import Station
> import qualified Train as T

> data Reservation = Reservation { resid :: Int
>                                , train :: T.Train
>                                , route :: Route
>                                , from  :: Station
>                                , to    :: Station
>                                , seats :: [ Int ]
>                                }
>       deriving (Show, Read)

Retrieves all reservations applicable to the given route segment on the given train.
Note that applicable in this case is defined as including the station.

> reservationsForTrainAndSegment :: [ Reservation ] -> (Route, T.Train, Station, Station) -> [ Reservation ]
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

> reserveSeats :: [ Reservation ] -> (Route, T.Train, Station, Station) -> Integer -> Maybe Reservation
> reserveSeats rs (r, t, src, dst) n
>       | minFreeSeatsExceeded = Nothing
>       | null bookedSeats     = Nothing
>       | otherwise            = Just $ Reservation newId t r src dst bookedSeats
>   where newId                = 1 + maximum (0:[ resid res  | res <- rs ])
>         applicableRs         = reservationsForTrainAndSegment rs (r, t, src, dst)
>         filledSeats          = fromIntegral $ sum [ length $ seats res | res <- applicableRs ]
>         minFreeSeatsExceeded = ((T.trainCapacity t) - filledSeats) < (T.minimumFreeSeats t)
>         bookedSeats          = reserveSeats' applicableRs (T.wagons t) n 0

We're almost done with all checks. We still need to find some place in the
train that has n contiguous seats available and return these.

rs: all applicable reservations
(w:ws) the remaining wagons
n: the desired count of seats
i: the starting seat number of this wagon

> reserveSeats' :: [ Reservation ] -> [ T.Wagon ] -> Integer -> Integer -> [ Int ]
> reserveSeats' rs (w:ws) n i
>       | hasSpace  = seatList
>       | otherwise = reserveSeats' rs ws n (i + T.seats w)
>   where allSeats      = [i .. i + T.seats w]
>         reservedSeats = map toInteger $ concat $ map (\r -> seats r) rs
>         availSeats    = allSeats \\ reservedSeats
>         hasSpace      = undefined
>         seatList      = undefined
> reserveSeats' _ _ _ _ = []
