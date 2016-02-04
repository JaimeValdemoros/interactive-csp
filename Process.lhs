> module Process
>     ( Event(..)
>     , Process(..)
>     , runEvent
>     , runEvents
>     ) where

> data Event = StringEvent String | NumEvent Integer deriving (Eq, Show)

> data Process = Stop 
>              | Prefix Event Process 
>                deriving (Show)

> runEvent :: Event -> Process -> Maybe Process
> runEvent e1 (Prefix e2 p) = if (e1 == e2) then Just p else Nothing

> runEvents :: [Event] -> Process -> Maybe Process
> runEvents [] p = Just p
> runEvents (e:es) p = runEvents es =<< runEvent e p
