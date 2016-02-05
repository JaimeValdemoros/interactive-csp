> module Parse
>      ( parseProc
>      , parseEv
>      ) where

> import Process
> import Text.Parsec
> import Text.Parsec.Numbers

> parser rule text = parse rule "(source)" text

> eitherToMaybe :: Either a b -> Maybe b
> eitherToMaybe (Left _) = Nothing
> eitherToMaybe (Right x) = Just x

> parseProc :: String -> Maybe Process
> parseProc = eitherToMaybe . parser (do {p <- parseProcess; eof; return p})

> parseEv :: String -> Maybe Event
> parseEv = eitherToMaybe . parser parseEvent

> parseEvent :: Parsec String () Event
> parseEvent = (parseIntegral >>= return . NumEvent)
>          <|> (many1 lower >>= return . StringEvent)

> parseStop :: Parsec String () Process
> parseStop = string "Stop" >> return Stop

> parsePrefix :: Parsec String () Process
> parsePrefix = try $ do
>     event <- parseEvent
>     spaces >> string "->" >> spaces
>     process <- parseProcess
>     return (Prefix event process)

> parseAbs :: Parsec String () Process
> parseAbs = try $ do
>                   char '$'
>                   first <- oneOf ['A'..'Z']
>                   char '$'
>                   p <- parseProcess
>                   return (Abs [first] p)

> parseIdent :: Parsec String () Process
> parseIdent = try $ do
>                     first <- oneOf ['A'..'Z']
>                     rest <- many letter
>                     return (Ident (first:rest))

> discardBrackets :: Parsec String st o -> Parsec String st o
> discardBrackets rule = (try $ do 
>                                string "("
>                                p <- rule
>                                string ")"
>                                return p)
>                    <|> rule

> parseProcess :: Parsec String () Process
> parseProcess = try $ discardBrackets $ choice [parsePrefix, parseStop, parseAbs, parseIdent]
