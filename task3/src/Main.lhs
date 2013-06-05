> import System.Console.GetOpt  ( OptDescr  ( Option )
>                               , ArgDescr  ( NoArg
>                                           , ReqArg
>                                           , OptArg
>                                           )
>                               , ArgOrder  ( Permute )
>                               , getOpt
>                               , usageInfo
>                               )
> import System.Environment  ( getArgs )
> import Control.Monad  ( when )
> import System.IO ( hPutStrLn
>                  , stdout 
>                  )
> import Database

The file used to persist and load our Database instances.

> dbFile = "db"

Our helpful usage message.

> usage = "Usage: TheCakeIsALie [OPTION...]"

Main

\begin{itemize}
\item Loads the database,
\item Parses all command line arguments,
\item Performs the selected action, and
\item Persists the (possibly changed) database.
\end{itemize}

TODO: Change main to handle commands as they arrive from stdin
instead of depending on command line options.

> main :: IO ()
> main = do
>   args <- getArgs
>   let (trafos, posargs, errs) = getOpt Permute options args
>   when (errs /= [])     $ ioError $ userError $ concat errs ++ usageInfo usage options
>   when (posargs /= [])  $ ioError $ userError $ usageInfo usage options
>   let opts       = foldl (flip id) defaultOptions trafos
>   do db <- readDatabase
>      hPutStrLn stdout $ printDatabase db
>      writeDatabase db
>      return ()

A quick wrapper for loading our database file. In particular, it's needed because
we need to specify the type of the returned variable.

> readDatabase :: IO Database
> readDatabase = do s <- readFile dbFile
>                   return (read s)

Likewise, write the specified database into our db file.

> writeDatabase :: Database -> IO ()
> writeDatabase db = writeFile dbFile $ show db

Command line argument handling courtesy of GetOpt and copied from Ondra's implementation
of task 1.

> data CmdlineOptions = CmdlineOptions
>   { cmdoptPrint          :: Bool
>   , cmdoptBootstrapFile  :: Maybe String
>   } deriving (Show, Eq)
>
> defaultOptions = CmdlineOptions
>   { cmdoptPrint          = False
>   , cmdoptBootstrapFile  = Nothing
>   }
>
> options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
> options =
>   [ Option  ['p'] ["print"]
>       (NoArg   (\os    -> os { cmdoptPrint     = True }))
>       "prints all data"
>   , Option  ['b'] ["bootstrap"]
>       (ReqArg  (\f os  -> os { cmdoptBootstrapFile  = Just f }) "FILE")
>       "file to execute before loading UI or executing input"
>   ]
