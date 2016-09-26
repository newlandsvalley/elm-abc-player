module VexTab.Config exposing (Config)

{-| configuration of VexTab

@docs Config
-}


{-| the configuration of the VexTab Canvas
-}
type alias Config =
    { canvasDivId : String
    , canvasX : Int
    , canvasY : Int
    , canvasWidth : Int
    , scale : Float
    }
