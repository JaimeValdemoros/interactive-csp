> import Process
> import Parse
> import System.Console.Haskeline

> interactProc :: IO ()
> interactProc = do
>     putStrLn "Type in a process definition"
>     minput <- runInputT defaultSettings $ getInputLine "> "
>     case minput of
>         Nothing -> return ()
>         Just s -> case parseProc s of
>                       Nothing -> return ()
>                       Just p -> iterateProc newContext p True

> isIdent :: Process -> Bool
> isIdent (Ident _) = True
> isIdent _ = False

> iterateProc :: Context -> Process -> Bool -> IO ()
> iterateProc _ Stop _ = putStrLn (show Stop)
> iterateProc c p allowError = do
>     putStrLn ("Current process state: " ++ show p)
>     putStrLn "Type in an event: "
>     minput <- runInputT defaultSettings $ getInputLine "> "
>     case interpretString c p minput of
>         Nothing -> putStrLn "Malformed input or invalid event" >> 
>                        if allowError then iterateProc c p False else return ()
>         Just (p', c') -> iterateProc c' p' True

> interpretString :: Context -> Process -> Maybe String
>                 -> Maybe (Process, Context)
> interpretString c p minput = do
>                 s <- minput
>                 if (null s) && isIdent p 
>                 then expandProc c p >>= (\p' -> return (p',c))
>                 else (parseEv s >>= runEvent c p)


