> module Process
>     ( Event(..)
>     , Process(..)
>     , Context
>     , getProc
>     , addToContext
>     , newContext
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

> getProc :: Context -> Ident -> Maybe Process
> getProc = flip M.lookup

> addToContext :: Ident -> Process -> Context -> Context
> addToContext = M.insert

> newContext :: Context
> newContext = M.empty

> data Process = Stop                      -- STOP
>              | Prefix Event Process      -- a -> P
>              | Abs Ident Process         -- \p . P
>              | Ident Ident               -- 'Count'

> instance Show Process where
>     show = pShow

> pShow :: Process -> String
> pShow Stop = "Stop"
> pShow (Prefix e p) = show e ++ " -> " ++ bracket p
> pShow (Abs s p) = "\\" ++ s ++ "." ++ bracket p
> pShow (Ident s) = s

> bracket :: Process -> String
> bracket Stop = pShow Stop
> bracket p@(Prefix _ _) = "(" ++ pShow p ++ ")"
> bracket a@(Abs _ _) = pShow a
> bracket i@(Ident _) = pShow i

> runEvent :: Context -> Process -> Event -> Maybe (Process, Context)
> runEvent _ Stop _ = Nothing
> runEvent c (Prefix e p) e2 = if (e == e2) 
>                               then Just (p, c)
>                               else Nothing
> runEvent c (Ident s) e = getProc c s >>= (\p -> runEvent c p e)
> runEvent c (Abs s p) e = runEvent (addToContext s p c) p e

