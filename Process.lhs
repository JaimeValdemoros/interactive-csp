> module Process
>     ( Event(..)
>     , Process(..)
>     , runEvent
>     , runEvents
>     ) where

> data Event = StringEvent String 
>            | NumEvent Integer 
>              deriving (Eq)

> instance Show Event where
>     show (StringEvent s) = s
>     show (NumEvent i) = show i

> data Process = Stop                      -- STOP
>              | Prefix Event Process      -- a -> P

> instance Show Process where
>     show Stop = "Stop"
>     show (Prefix e Stop) = show e ++ " -> Stop"
>     show (Prefix e p) = show e ++ " -> (" ++ show p ++ ")"

> runEvent :: Event -> Process -> Maybe Process
> runEvent e Stop = Nothing
> runEvent e1 (Prefix e2 p) = if (e1 == e2) 
>                             then Just p
>                             else Nothing

> runEvents :: [Event] -> Process -> Maybe Process
> runEvents [] p = Just p
> runEvents (e:es) p = runEvents es =<< runEvent e p
