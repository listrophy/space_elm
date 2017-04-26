module Main exposing (main)

import Html exposing (..)
import Dict
import Json.Decode as JD
import Json.Encode as JE
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
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Position =
    { x : Float
    , y : Float
    }


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
    , requestRandomAsteroidPositions
    )


initCable : ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable"
        |> ActionCable.withDebug True
        |> ActionCable.onWelcome (Just CableConnected)
        |> ActionCable.onDidReceiveData (Just DataReceived)



-- Randomness request


asteroidPositionGenerator : Random.Generator Position
asteroidPositionGenerator =
    let
        xGenerator =
            Random.float 500.0 1500.0

        yGenerator =
            Random.float -220.0 220.0
    in
        Random.map2 Position xGenerator yGenerator


requestRandomAsteroidPositions : Cmd Msg
requestRandomAsteroidPositions =
    Random.generate GenerateAsteroid asteroidPositionGenerator
        |> List.repeat 12
        |> Cmd.batch



-- UPDATE


type Msg
    = CableMsg ACMsg.Msg
    | Tick Time.Time
    | CableConnected ()
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | DataReceived ID.Identifier JD.Value
    | GenerateAsteroid Position


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
            ( fire model, Cmd.none )

        KeyDown keyCode ->
            ( { model | keysDown = Set.insert keyCode model.keysDown }
            , Cmd.none
            )

        KeyUp keyCode ->
            ( { model | keysDown = Set.remove keyCode model.keysDown }
            , Cmd.none
            )

        DataReceived _ jsonValue ->
            ( dataReceived jsonValue model, Cmd.none )

        GenerateAsteroid position ->
            ( { model | asteroids = position :: model.asteroids }
            , Cmd.none
            )



-- Joining the game


joinGame : Model -> ( Model, Cmd Msg )
joinGame model =
    case subscribeToGame model of
        Ok model_cmd ->
            model_cmd

        Err err ->
            ( { model
                | error = Just <| ActionCable.errorToString err
              }
            , Cmd.none
            )


subscribeToGame :
    Model
    -> Result ActionCable.ActionCableError ( Model, Cmd Msg )
subscribeToGame model =
    let
        setCable ( newCable, cmd ) =
            ( { model | cable = newCable }, cmd )
    in
        ActionCable.subscribeTo identifier model.cable
            |> Result.map setCable


identifier : ID.Identifier
identifier =
    ID.newIdentifier "GamesChannel" []



-- receiving the updated score


dataReceived : JD.Value -> Model -> Model
dataReceived json model =
    json
        |> JD.decodeValue (JD.field "score" JD.int)
        |> Result.toMaybe
        |> (\score -> { model | score = score })



-- Fire!


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



-- AnimationFrame loop


tick : Time.Time -> Model -> ( Model, Cmd Msg )
tick time model =
    model
        |> moveShipAndLaser
        |> moveAsteroids
        |> blowUpAsteroid
        |> postScore
        |> (\( m, a, cmd ) -> ( m, regenerateAsteroids a cmd ))



-- Move Ship


moveShipAndLaser : Model -> Model
moveShipAndLaser ({ myPosition } as model) =
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



-- Move asteroids


moveAsteroids : Model -> ( Model, List Position )
moveAsteroids model =
    let
        move { x, y } =
            { x = x - 1, y = y }

        ( kept, past ) =
            model.asteroids
                |> List.map move
                |> List.partition (\pos -> pos.x > leftEdge - 50)
    in
        ( { model | asteroids = kept }, past )



-- Blow up asteroids


blowUpAsteroid :
    ( Model, List Position )
    -> ( Model, List Position, List Position )
blowUpAsteroid ( model, past ) =
    case model.laser of
        Nothing ->
            ( model, past, [] )

        Just laser ->
            let
                ( blownUp, keep ) =
                    List.partition
                        (detectCollision laser)
                        model.asteroids
            in
                if List.length blownUp > 0 then
                    ( { model | asteroids = keep, laser = Nothing }
                    , past
                    , blownUp
                    )
                else
                    ( model, past, [] )


detectCollision : Position -> Position -> Bool
detectCollision laser asteroid =
    asteroidRadius > (sqrt <| (asteroid.x - laser.x) ^ 2 + (asteroid.y - laser.y) ^ 2)



-- Post a new score


postScore :
    ( Model, List Position, List Position )
    -> ( Model, List Position, Cmd Msg )
postScore ( model, past, blownUp ) =
    ( model
    , List.append past blownUp
    , sendScoreUpdate (List.length blownUp) model.cable
    )


sendScoreUpdate : Int -> ActionCable Msg -> Cmd Msg
sendScoreUpdate int cable =
    if int > 0 then
        Result.withDefault Cmd.none <|
            ActionCable.perform
                "scoreUpdate"
                [ ( "score", JE.int int ) ]
                identifier
                cable
    else
        Cmd.none



-- Regenerate asteroids that were blown up or past the screen


regenerateAsteroids : List Position -> Cmd Msg -> Cmd Msg
regenerateAsteroids asteroids existingCmd =
    let
        regen _ =
            Random.generate
                GenerateAsteroid
                asteroidPositionGenerator
    in
        asteroids
            |> List.map regen
            |> (::) existingCmd
            |> Cmd.batch



-- geometry data


asteroidRadius : Float
asteroidRadius =
    30.0


leftEdge : Float
leftEdge =
    -480.0



-- VIEW


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
          , shipView model.myPosition
          ]
        , List.map asteroidView model.asteroids
        , Maybe.withDefault [] <|
            Maybe.map (laserView >> List.singleton) model.laser
        ]


space : C.Form
space =
    C.rect 1000 500
        |> C.filled Color.black


connectedSignal : Model -> C.Form
connectedSignal { cable } =
    let
        colorFill =
            if Dict.isEmpty <| ActionCable.subscriptions cable then
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
        |> C.text
        |> C.move (( 420.0, 240 ))


shipView : Float -> C.Form
shipView myPosition =
    C.polygon [ ( -10, -5 ), ( -10, 5 ), ( 10, 0 ) ]
        |> C.filled Color.white
        |> C.move ( leftEdge, (250.0 / 100.0 * myPosition) )


laserView : Position -> C.Form
laserView laser =
    C.rect 20.0 2.0
        |> C.filled Color.white
        |> C.move ( laser.x, laser.y )


asteroidView : Position -> C.Form
asteroidView position =
    C.ngon 7 asteroidRadius
        |> C.filled Color.gray
        |> C.move ( position.x, position.y )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionCable.listen CableMsg model.cable
        , AF.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
