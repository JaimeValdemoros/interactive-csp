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

> parseProc :: String -> Maybe ProcessDef
> parseProc = eitherToMaybe . parser (do {p <- parseProcessDef; eof; return p})

> parseEv :: String -> Maybe Event
> parseEv = eitherToMaybe . parser parseEvent

> parseEvent :: Parsec String () Event
> parseEvent = (parseIntegral >>= return . NumEvent)
>          <|> (many1 lower >>= return . StringEvent)

> parseStop :: Parsec String () ProcessDef
> parseStop = string "Stop" >> return Stop

> parsePrefix :: Parsec String () ProcessDef
> parsePrefix = try $ do
>     event <- parseEvent
>     spaces >> string "->" >> spaces
>     process <- parseProcessDef
>     return (Prefix event process)

> parseAbs :: Parsec String () ProcessDef
> parseAbs = try $ do
>                   char '$'
>                   first <- oneOf ['A'..'Z']
>                   char '.'
>                   p <- parseProcessDef
>                   return (Abs [first] p)

> parseIdent :: Parsec String () ProcessDef
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

> parseProcessDef :: Parsec String () ProcessDef
> parseProcessDef = try $ discardBrackets $ choice [parsePrefix, parseStop, parseAbs, parseIdent]
