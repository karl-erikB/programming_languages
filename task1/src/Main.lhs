This is where it all comes together... and it starts with imports.

> import Data.Word  ( Word8
>                   )
> import Data.Either.Unwrap  ( fromLeft
>                            , fromRight
>                            , isLeft
>                            , isRight
>                            )
> import System.Environment  ( getArgs
>                            , getProgName
>                            )
> import System.IO  ( Handle
>                   , hIsEOF
>                   , hIsTerminalDevice
>                   , hFlush
>                   , hGetLine
>                   , hPutStrLn
>                   , hPutStr
>                   , stderr
>                   , stdin
>                   , stdout
>                   )
> import qualified CakeDisplay    as Di
> import qualified CakeParser     as Pa
> import qualified CakePrinter    as Pr
> import qualified CakeStack      as Sk
> import qualified CakeState      as Se
> import qualified CakeValue      as V
> import qualified CakeEvaluator  as Ev
> import qualified RepeatState    as RS

The \textit{main} function fetches and parses the command-line arguments and
then enters the processing loop.

> main :: IO ()
> main = do
>   args <- getArgs
>   let evalState  =  if    "--step-by-step" `elem` args
>                     then  evalStateSBS
>                     else  evalStateDirect
>   let showStack  = "--show-stack" `elem` args
>   let display    = Di.emptyDisplay
>   let stack      = (Sk.Stack [])
>   if "--help" `elem` args
>   then do
>     outputUsage
>   else do
>     pl <- processingLoop evalState showStack stdin (Se.State display stack)
>     return ()

The processing loop reads reads a line from standard input, parses it, pushes
the parsed stack onto the calculator's stack, then evaluates the latter.
Finally, it outputs the calculator's display. The command-line option
\verb|--step-by-step| will cause Cakeulator to pretty-print the stack before
each evaluation; the \verb|--show-stack| option will pretty-print the stack
along with the display after each line of input has been processed.

> processingLoop ::  (Se.State -> IO (Either String Se.State))
>                    -> Bool -> Handle -> Se.State
>                    -> IO (Either () ())
> processingLoop evalState showStack inhdl st8 = do
>   res <- processOne evalState showStack inhdl st8
>   if RS.isRepeat res
>   then
>     processingLoop evalState showStack inhdl $ RS.fromRepeat res
>   else if RS.isStop res
>   then
>     return $ Right ()
>   else
>     return $ Left ()
>
> processOne ::  (Se.State -> IO (Either String Se.State))
>                -> Bool -> Handle -> Se.State
>                -> IO (RS.RepeatState Se.State () ())
> processOne evalState showStack inhdl st8 = do
>   putStrLn $ Di.prettyFormatBox $ Se.display st8
>   if showStack then do
>     putStrLn $ Pr.prettyStack $ Se.stack st8
>   else do
>     return ()
>   itd <- hIsTerminalDevice stdout
>   if itd then do
>     putStr "> "
>     hFlush stdout
>   else do
>     return ()
>   iseof <- hIsEOF inhdl
>   if iseof
>   then do
>     return $ RS.Stop ()
>   else do
>     ln <- hGetLine inhdl
>     let pres = Pa.parse ln
>     if isLeft pres
>     then do
>       hPutStrLn stderr $ "error parsing input: " ++ fromLeft pres
>       return $ RS.Failure ()
>     else do
>       let (Sk.Stack vs)  = fromRight pres
>       let newstk         = Sk.multipush vs $ Se.stack st8
>       let newst8         = Se.swizzleStack st8 newstk
>       evalres <- evalState newst8
>       if isLeft evalres
>       then do
>         hPutStrLn stderr $ "error evaluating: " ++ fromLeft evalres
>         return $ RS.Failure ()
>       else do
>         return $ RS.Repeat $ fromRight evalres

\textit{evalStateSBS} invokes \textit{evaluateStepping} to perform the
step-by-step evaluation.

> evalStateSBS :: Se.State -> IO (Either String Se.State)
> evalStateSBS = Ev.evaluateStepping

\textit{evalStateDirect} invokes \textit{evaluate}, ignoring the handle and the
I/O monad.

> evalStateDirect :: Se.State -> IO (Either String Se.State)
> evalStateDirect st8 = return $ Ev.evaluate st8

\textit{outputUsage} outputs information about Cakeulator's command-line
parameters.

> outputUsage :: IO ()
> outputUsage = do
>   progname <- getProgName
>   hPutStr stderr $
>     "Usage: " ++ progname ++ " OPTIONS\n" ++
>     "  --step-by-step   Output the stack after every evaluation step.\n" ++
>     "  --show-stack     Output the stack alongside the display after every\n" ++
>     "                   evaluated line.\n" ++
>     "\n" ++
>     "Processing from files is currently not supported. Redirect stdin instead.\n"
