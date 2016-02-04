> import Process
> import Parse

> interactProc :: IO ()
> interactProc = do
>     putStrLn "Type in a process definition"
>     s <- getLine
>     case parseProc s of
>         Nothing -> return ()
>         Just p -> iterateProc p

> iterateProc :: Process -> IO ()
> iterateProc Stop = putStrLn (show Stop)
> iterateProc p = do
>     putStrLn ("Current process state: " ++ show p)
>     putStr "Type in an event: "
>     s <- getLine
>     if (s == "") then return () else
>       case (parseEv s >>= flip runEvent p) of
>           Nothing -> putStrLn "Malformed input or invalid event" >> iterateProc p
>           Just p' -> iterateProc p'

