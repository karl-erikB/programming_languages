> import Control.Monad  ( when )
> import System.Directory ( copyFile
>                         , removeFile
>                         )
> import System.IO ( hGetLine
>                  , hIsEOF
>                  , hPutStrLn
>                  , stdin
>                  , stdout 
>                  )
> import Database

The file used to persist and load our Database instances.

> dbFile = "db"

> main :: IO ()
> main = do
>   db <- readDatabase
>   dirtyDb <- mainLoop db
>   writeDatabase dirtyDb
>   return ()

A quick wrapper for loading our database file. In particular, it's needed because
we need to specify the type of the returned variable.

> readDatabase :: IO Database
> readDatabase = do s <- readFile dbFile
>                   return (read s)

Likewise, write the specified database into our db file.

> writeDatabase :: Database -> IO ()
> writeDatabase db = do
>   writeFile tmpFile $ show db
>   copyFile tmpFile dbFile
>   removeFile tmpFile
>   where tmpFile = '_' : dbFile

> mainLoop :: Database -> IO Database
> mainLoop db = do
>       iseof <- hIsEOF stdin
>       if iseof
>           then do
>               return db
>           else do
>               ln <- hGetLine stdin
>               if ln == "quit"
>                   then do
>                       return db
>                   else do
>                       let (output, dirtyDb) = processCommand db ln
>                       hPutStrLn stdout output
>                       mainLoop dirtyDb

> usage :: String
> usage = "Usage:\n" ++ concat [ "    " ++ line ++ "\n" | line <- lines ]
>   where lines =
>           [ "print"
>           , "reserve train from to count"
>           , "delreserve id"
>           , "minfreemaxreserved from to"
>           , "seatreservations train seat"
>           , "maxgroupsize from to"
>           , "quit"
>           ]

Given a database and some input command, we process the given command
and return a string to print as well as the possibly modified database.

> processCommand :: Database -> String -> (String, Database)
> processCommand db cmd =
>   case cmds !! 0 of
>   "print"              -> (printDatabase db, db)
>   "reserve"            -> addReservation db cmds
>   "delreserve"         -> undefined -- TODO
>   "minfreemaxreserved" -> undefined -- TODO
>   "seatreservations"   -> (printReservations db cmds, db)
>   "maxgroupsize"       -> undefined -- TODO
>   otherwise            -> (usage, db)
>   where cmds = words cmd
