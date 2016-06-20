port module FileIO.Ports exposing (..)

-- outgoing ports (for commands to javascript)

port requestLoadFile : () -> Cmd msg

-- incoming ports (for subscriptions from javascript)

{-| Has the file been loaded OK? -}
port fileLoaded : (String -> msg) -> Sub msg


