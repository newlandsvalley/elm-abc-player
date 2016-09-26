module VexTab exposing (Model, Msg(RequestRenderScore), init, update, subscriptions)

{-|
Interface to VexTab native functionality

#types

@docs Model, Msg

#functions

@docs init, update, subscriptions


-}

import VexTab.Ports as VexTab


{-| in the model we only have a possible error message
-}
type alias Model =
    { text : Maybe String
    , error : Maybe String
    }


{-| Initialise the model and initialise VexTab with the name of the canvas element
-}
init divName =
    ( Model Nothing Nothing
    , VexTab.initialise divName
    )


{-| all messages are internal to the Module except for RequestRenderScore
-}
type Msg
    = InitialisedVexTab (Maybe String)
    | RequestRenderScore String
    | ResponseScoreRendered (Maybe String)


{-| update the VexTab module
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialisedVexTab maybeError ->
            let
                newModel =
                    saveError maybeError model
            in
                ( newModel
                , Cmd.none
                )

        RequestRenderScore text ->
            ( { model | text = Just text, error = Nothing }
            , VexTab.requestRender text
            )

        ResponseScoreRendered maybeError ->
            let
                newModel =
                    saveError maybeError model
            in
                ( newModel
                , Cmd.none
                )



{- save any error from the VexFlow API in the model -}


saveError : Maybe String -> Model -> Model
saveError maybeError model =
    case maybeError of
        Just e ->
            { model | error = maybeError }

        _ ->
            model


initialisedSub : Sub Msg
initialisedSub =
    VexTab.initialised InitialisedVexTab


renderedSub : Sub Msg
renderedSub =
    VexTab.rendered ResponseScoreRendered


{-| subscriptions that can be used with VexTab
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ initialisedSub
        , renderedSub
        ]
