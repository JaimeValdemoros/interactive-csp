> module Process
>     ( Event(..)
>     , Process(..)
>     , runEvent
>     ) where

> import qualified Data.Map.Lazy as M

> data Event = StringEvent String 
>            | NumEvent Integer 
>              deriving (Eq)

> instance Show Event where
>     show (StringEvent s) = s
>     show (NumEvent i) = show i

> 

> data Process = Stop                      -- STOP
>              | Prefix Event Process      -- a -> P
>              | Abs Ident Process         -- \p . P
>              | Ident Ident               -- 'Count'

> type Ident = String
> type Context = M.Map Ident Process

> instance Eq Process where
>     (==) Stop Stop = True
>     (==) (Ident s1) (Ident s2) = s1 == s2
>     (==) _ _ = False

> instance Show Process where
>     show Stop = "Stop"
>     show (Prefix e Stop) = show e ++ " -> Stop"
>     show (Prefix e p) = show e ++ " -> (" ++ show p ++ ")"

> runEvent :: Context -> Event -> Process -> Maybe Process
> runEvent _ e Stop = Nothing
> runEvent _ e1 (Prefix e2 p) = if (e1 == e2) 
>                               then Just p
>                               else Nothing
> runEvent c e (Ident s) = runEvent c e =<< getProc c s

> getProc :: Context -> Ident -> Maybe Process
> getProc = flip M.lookup

