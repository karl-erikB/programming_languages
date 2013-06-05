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
>       if iseof then do
>           return db
>       else do
>           ln <- hGetLine stdin
>           hPutStrLn stdout $ printDatabase db
>           return db
