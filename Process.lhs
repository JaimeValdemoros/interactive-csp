> module Process
>     ( Event(..)
>     , ProcessDef(..)
>     , Process
>     , Context
>     , addToContext
>     , newContext
>     , expandProc
>     , runEvent
>     ) where

> import qualified Data.Map.Lazy as M
> import Data.List(elem)

> data Event = StringEvent String 
>            | NumEvent Integer 
>              deriving (Eq)

> instance Show Event where
>     show (StringEvent s) = s
>     show (NumEvent i) = show i

> type Ident = String

> data ProcessDef = Stop                      -- STOP
>                 | Prefix Event ProcessDef      -- a -> P
>                 | Abs Ident ProcessDef         -- \p . P
>                 | Ident Ident               -- 'Count'
>                 | NormalProc [Event] ProcessDef

> instance Show ProcessDef where
>     show = (++) "Proc " . pShow

> pShow :: ProcessDef -> String
> pShow Stop = "Stop"
> pShow (Prefix e p) = show e ++ 
>                      " -> " ++ 
>                      case p of
>                         (Prefix _ _) -> pShow p
>                         _            -> bracket p
> pShow (Abs s p) = "\\" ++ s ++ "." ++ bracket p
> pShow (Ident s) = s

> bracket :: ProcessDef -> String
> bracket Stop = pShow Stop
> bracket p@(Prefix _ _) = "(" ++ pShow p ++ ")"
> bracket a@(Abs _ _) = pShow a
> bracket i@(Ident _) = pShow i

> type Context = M.Map Ident ProcessDef

> lookupProc :: Context -> Ident -> Maybe ProcessDef
> lookupProc = flip M.lookup

> expandProc :: Process -> Maybe Process
> expandProc (c, Ident s) = lookupProc c s >>= (\p -> return (c,p))
> expandProc p = Just p

> addToContext :: Ident -> ProcessDef -> Context -> Context
> addToContext = M.insert

> newContext :: Context
> newContext = M.empty

> type Process = (Context, ProcessDef) -- implement nondeterminism

> normalise :: Process -> Process
> normalise (c, Stop) = (c, NormalProc [] Stop)
> normalise (c, (Prefix e p)) = (c, NormalProc [e] p)
> normalise (c, (Ident s)) = case lookupProc c s of
>                                Nothing -> error "Undefined ident"
>                                Just p' -> normalise (c, p')
> normalise (c, (Abs s p)) = normalise ((addToContext s p c), p)

> canRun :: Process -> Event -> Bool
> canRun p e = (\(c, NormalProc es _) -> elem e es) $ normalise p

> runEvent :: Process -> Event -> Maybe Process
> runEvent p e = flip ($) (normalise p) (\(c, NormalProc es p') ->
>                    if elem e es then Just (c, p') else Nothing)

