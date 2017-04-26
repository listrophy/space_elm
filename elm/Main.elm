module Main exposing (main)

import Html exposing (..)
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


type Key
    = ArrowDown
    | ArrowUp
    | SpaceBar


type alias Model =
    { myPosition : Float
    , laser : Maybe Position
    , asteroids : List Position
    , score : Maybe Int
    , error : Maybe String
    , cable : ActionCable Msg
    , keysDown : Set.Set Keyboard.KeyCode
    }


init : ( Model, Cmd Msg )
init =
    ( { myPosition = 0.0
      , laser = Nothing
      , asteroids = []
      , score = Nothing
      , error = Nothing
      , cable = initCable
      , keysDown = Set.empty
      }
    , Cmd.none
      -- TODO: generate asteroids
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
            tick time model

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
    json
        |> JD.decodeValue (JD.field "score" JD.int)
        |> Result.toMaybe
        |> (\score -> { model | score = score })


fire : Model -> Model
fire ({ laser, myPosition } as model) =
    let
        newLaser =
            Just <| Position leftEdge (2.5 * myPosition)
    in
        case laser of
            Nothing ->
                { model | laser = newLaser }

            Just laser ->
                if laser.x < 500 then
                    model
                else
                    { model | laser = newLaser }


leftEdge : Float
leftEdge =
    -480.0


tick : Time.Time -> Model -> ( Model, Cmd Msg )
tick time model =
    model
        |> doMove
        |> flip (!) []


doMove : Model -> Model
doMove ({ myPosition } as model) =
    let
        moveMe =
            if Set.member 38 model.keysDown then
                2.0
            else if Set.member 40 model.keysDown then
                -2.0
            else
                0.0
    in
        { model
            | myPosition = myPosition + moveMe
            , laser = Maybe.map moveLaser model.laser
        }


moveLaser : Position -> Position
moveLaser { x, y } =
    { x = x + 10, y = y }


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
    Element.toHtml <|
        C.collage 1000 500 <|
            gameItems model


gameItems : Model -> List C.Form
gameItems model =
    List.concat
        [ [ space
          , connectedSignal model
          , scoreView model.score
          , meView model.myPosition
          ]
        , List.filterMap identity [ meLaserView model.laser ]
        ]


space : C.Form
space =
    C.filled Color.black <| C.rect 1000 500


connectedSignal : Model -> C.Form
connectedSignal model =
    let
        colorFill =
            if Dict.isEmpty <| ActionCable.subscriptions model.cable then
                Color.gray
            else
                Color.green
    in
        C.circle 5.0
            |> C.filled colorFill
            |> C.move ( 490, -240 )


scoreView : Maybe Int -> C.Form
scoreView int =
    int
        |> Maybe.map toString
        |> Maybe.withDefault "?"
        |> (++) "Score: "
        |> Text.fromString
        |> Text.color Color.white
        |> Element.rightAligned
        |> C.toForm
        |> C.move (( 420.0, 240 ))


meView : Float -> C.Form
meView myPosition =
    C.polygon [ ( -10, -5 ), ( -10, 5 ), ( 10, 0 ) ]
        |> C.filled Color.white
        |> C.move ( leftEdge, (250.0 / 100.0 * myPosition) )


meLaserView : Maybe Position -> Maybe C.Form
meLaserView =
    let
        laserView laser =
            C.rect 20.0 2.0
                |> C.filled Color.white
                |> C.move ( laser.x, laser.y )
    in
        Maybe.map laserView


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionCable.listen CableMsg model.cable
        , AF.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
