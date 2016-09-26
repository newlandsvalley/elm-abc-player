port module VexTab.Ports exposing (..)

-- outgoing ports (for commands to javascript)

import VexTab.Config exposing (Config)


port initialise : Config -> Cmd msg


port requestRender : String -> Cmd msg



-- incoming ports (for subscriptions from javascript)


{-| is VexTab initialised - maybe there's an error message?
-}
port initialised : (Maybe String -> msg) -> Sub msg


{-| Have we rendered the score - maybe there's an error message?
-}
port rendered : (Maybe String -> msg) -> Sub msg
