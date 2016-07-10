port module FileIO.Ports exposing (..)

type alias Filespec =
  {
    contents : String
  , name : String
  }

-- outgoing ports (for commands to javascript)

port requestLoadFile : () -> Cmd msg

port requestSaveFile : Filespec -> Cmd msg

-- incoming ports (for subscriptions from javascript)

{-| Has the file been loaded OK? -}
port fileLoaded : (Maybe Filespec -> msg) -> Sub msg

{-| Has the file been saved OK? -}
port fileSaved : (Bool -> msg) -> Sub msg


