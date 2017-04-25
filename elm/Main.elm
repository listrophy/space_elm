module Main exposing (main)

import Html exposing (..)
import Html.Attributes as A exposing (href)
import Dict
import Json.Decode as JD
import ActionCable exposing (ActionCable)
import ActionCable.Msg as ACMsg
import ActionCable.Identifier as ID
import Collage as C
import Element
import Color
import Text
import Time
import AnimationFrame as AF
import Keyboard
import Set


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Player =
    { position : Float
    , laser : Maybe Position
    }


type alias Asteroid =
    { position : Position
    }


type Key
    = ArrowDown
    | ArrowUp
    | SpaceBar


type alias Model =
    { me : Player
    , error : Maybe String
    , cable : ActionCable Msg
    , keysDown : Set.Set Keyboard.KeyCode
    }


init : ( Model, Cmd Msg )
init =
    ( { me = Player 0.0 Nothing
      , error = Nothing
      , cable = initCable
      , keysDown = Set.empty
      }
    , Cmd.none
    )


type Msg
    = CableMsg ACMsg.Msg
    | Tick Time.Time
    | CableConnected ()
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | DataReceived ID.Identifier JD.Value


initCable : ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable"
        |> ActionCable.withDebug True
        |> ActionCable.onWelcome (Just CableConnected)
        |> ActionCable.onDidReceiveData (Just DataReceived)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CableMsg msg_ ->
            ActionCable.update msg_ model.cable
                |> Tuple.mapFirst (\c -> { model | cable = c })

        CableConnected _ ->
            joinGame model

        Tick time ->
            ( tick time model, Cmd.none )

        KeyDown 32 ->
            fire model ! []

        KeyDown keyCode ->
            ( { model | keysDown = Set.insert keyCode model.keysDown }, Cmd.none )

        KeyUp keyCode ->
            ( { model | keysDown = Set.remove keyCode model.keysDown }, Cmd.none )

        DataReceived _ jsonValue ->
            ( dataReceived jsonValue model, Cmd.none )


dataReceived : JD.Value -> Model -> Model
dataReceived json model =
    model


fire : Model -> Model
fire ({ me } as model) =
    case me.laser of
        Nothing ->
            { model | me = { me | laser = Just <| Position leftEdge (2.5 * me.position) } }

        Just laser ->
            if laser.x < 500 then
                model
            else
                { model | me = { me | laser = Just <| Position leftEdge (2.5 * me.position) } }


leftEdge : Float
leftEdge =
    -480.0


tick : Time.Time -> Model -> Model
tick time model =
    model
        |> doMove


doMove : Model -> Model
doMove ({ me } as model) =
    let
        moveMe =
            if Set.member 38 model.keysDown then
                2.0
            else if Set.member 40 model.keysDown then
                -2.0
            else
                0.0

        newMe =
            { me
                | position = me.position + moveMe
                , laser = Maybe.map moveLaser me.laser
            }
    in
        { model | me = newMe }


moveLaser : Position -> Position
moveLaser { x, y } =
    { x = x + 5, y = y }


identifier : ID.Identifier
identifier =
    ID.newIdentifier "GamesChannel" []


joinGame : Model -> ( Model, Cmd Msg )
joinGame model =
    case subscribeToGame model of
        Ok model_cmd ->
            model_cmd

        Err err ->
            ( { model | error = Just <| ActionCable.errorToString err }, Cmd.none )


subscribeToGame : Model -> Result ActionCable.ActionCableError ( Model, Cmd Msg )
subscribeToGame model =
    ActionCable.subscribeTo identifier model.cable
        |> Result.map (\( cable, cmd ) -> ( { model | cable = cable }, cmd ))



--


view : Model -> Html Msg
view model =
    div [ A.style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "height", "100%" ) ] ]
        [ div []
            [ text <|
                if Dict.isEmpty <| ActionCable.subscriptions model.cable then
                    "unsubscribed"
                else
                    "subscribed!"
            , text <| Maybe.withDefault "" model.error
            ]
        , div [ A.style [ ( "flex-grow", "2" ) ] ]
            [ gameView model ]
        ]


gameView : Model -> Html Msg
gameView model =
    Element.toHtml <|
        C.collage 1000 500 <|
            gameItems model


noGame : List C.Form
noGame =
    [ space
    , C.toForm <| Element.centered <| Text.color Color.lightGray <| Text.fromString "game not loaded"
    ]


gameItems : Model -> List C.Form
gameItems model =
    List.concat
        [ [ space ]
        , [ meView model.me ]
        , List.filterMap identity [ meLaserView model.me ]
        ]


space : C.Form
space =
    C.filled Color.black <| C.rect 1000 500


meView : Player -> C.Form
meView me =
    C.square 20
        |> C.filled Color.white
        |> C.move ( leftEdge, (250.0 / 100.0 * me.position) )


meLaserView : Player -> Maybe C.Form
meLaserView =
    let
        laserView laser =
            C.rect 20.0 2.0
                |> C.filled Color.white
                |> C.move ( laser.x, laser.y )
    in
        .laser >> Maybe.map laserView


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionCable.listen CableMsg model.cable
        , AF.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
