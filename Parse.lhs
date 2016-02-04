> import Process
> import Text.Parsec
> import Text.Parsec.Numbers

> parser rule text = parse rule "(source)" text

> parseStop :: Parsec String () Process
> parseStop = do
>     string "Stop"
>     return Stop

> parseEvent :: Parsec String () Event
> parseEvent = (parseIntegral >>= return . NumEvent)
>          <|> (many1 lower >>= return . StringEvent)

> parsePrefix :: Parsec String () Process
> parsePrefix = do
>     event <- parseEvent
>     spaces >> string "->" >> spaces
>     process <- parseProcess
>     return (Prefix event process)

> parseProcess :: Parsec String () Process
> parseProcess = do
>     p <-choice [parsePrefix, parseStop]
>     eof
>     return p
