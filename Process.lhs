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

> type Ident = String
> type Context = M.Map Ident Process

> getProc :: Ident -> Context -> Maybe Process
> getProc = M.lookup

> addToContext :: Ident -> Process -> Context -> Context
> addToContext = M.insert

> data Process = Stop                      -- STOP
>              | Prefix Event Process      -- a -> P
>              | Abs Ident Process         -- \p . P
>              | Ident Ident               -- 'Count'

> instance Show Process where
>     show = pShow

> pShow :: Process -> String
>     show Stop = "Stop"
>     show (Prefix e p) = show e ++ " -> " ++ bracket p
>     show (Abs s p) = "\\" ++ s ++ "." ++ bracket p
>     show (Ident s) = s

> bracket :: Process -> String
> bracket Stop = pShow Stop
> bracket p@(Prefix _ _) = "(" ++ pShow p ++ ")"
> bracket a@(Abs _ _) = pShow a
> bracket i@(Ident _) = pShow i

> runEvent :: Event -> Process -> Context -> Maybe (Process, Context)
> runEvent e Stop _ = Nothing
> runEvent e (Prefix e2 p) c = if (e == e2) 
>                               then Just (p,c)
>                               else Nothing
> runEvent e (Ident s) c = getProc s c >>= (\p -> runEvent e p c)
> runEvent e (Abs s p) c = runEvent e p (addToContext s p c)

