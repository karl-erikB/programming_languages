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

> usage = "Usage: Figure it out yourself damnit"

> main :: IO ()
> main = do
>   args <- getArgs
>   let (trafos, posargs, errs) = getOpt Permute options args
>   when (errs /= [])     $ ioError $ userError $ concat errs ++ usageInfo usage options
>   when (posargs /= [])  $ ioError $ userError $ usageInfo usage options
>   let opts       = foldl (flip id) defaultOptions trafos
>   do db <- readDatabase
>      hPutStrLn stdout $ show db
>      return()

> readDatabase :: IO Database
> readDatabase = do s <- readFile "db"
>                   return (read s)

> data CmdlineOptions = CmdlineOptions
>   { cmdoptPrint          :: Bool
>   , cmdoptBootstrapFile  :: Maybe String
>   } deriving (Show, Eq)
>
> defaultOptions = CmdlineOptions
>   { cmdoptPrint          = False
>   , cmdoptBootstrapFile  = Nothing
>   }

> options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
> options =
>   [ Option  ['p'] ["print"]
>       (NoArg   (\os    -> os { cmdoptPrint     = True }))
>       "prints all data"
>   , Option  ['b'] ["bootstrap"]
>       (ReqArg  (\f os  -> os { cmdoptBootstrapFile  = Just f }) "FILE")
>       "file to execute before loading UI or executing input"
>   ]
