> import Process
> import Parse

> interactProc :: IO ()
> interactProc = do
>     putStrLn "Type in a process definition"
>     s <- getLine
>     case parseProc s of
>         Nothing -> return ()
>         Just p -> iterateProc newContext p

> iterateProc :: Context -> Process -> IO ()
> iterateProc _ Stop = putStrLn (show Stop)
> iterateProc c p = do
>     putStrLn ("Current process state: " ++ show p)
>     putStr "Type in an event: "
>     s <- getLine
>     if (s == "") then return () else
>       case (parseEv s >>= runEvent c p) of
>           Nothing -> putStrLn "Malformed input or invalid event" >> iterateProc c p
>           Just (p', c') -> iterateProc c' p'

