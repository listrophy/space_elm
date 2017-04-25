module Main exposing (main)

import Html exposing (..)
import Html.Attributes as A exposing (href)
import Http
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


type alias Game =
    { me : Player
    , keysDown : Set.Set Keyboard.KeyCode
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Player =
    { id : PlayerId
    , position : Float
    , laser : Maybe Position
    }


type alias Asteroid =
    { id : AsteroidId
    , position : Position
    }


type Key
    = ArrowDown
    | ArrowUp
    | SpaceBar


type alias Model =
    { game : Maybe Game
    , myId : Maybe Int
    , error : Maybe String
    , cable : ActionCable Msg
    }


type Msg
    = CableMsg ACMsg.Msg
    | ReceiveMyId (Result Http.Error Int)
    | Tick Time.Time
    | CableConnected ()
    | GameJoined ID.Identifier
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | DataReceived ID.Identifier JD.Value


type alias AsteroidId =
    Int


type alias PlayerId =
    Int


type RemoteMsg
    = TheyMoved Player
    | TheyCollided PlayerId AsteroidId
    | TheyShot PlayerId AsteroidId
    | TheyReset Asteroid


init : ( Model, Cmd Msg )
init =
    ( { game = Nothing
      , error = Nothing
      , myId = Nothing
      , cable = initCable
      }
    , fetchUserId
    )


fetchUserId : Cmd Msg
fetchUserId =
    Http.get "/user.json" (JD.field "id" JD.int)
        |> Http.send ReceiveMyId


initGame : PlayerId -> Maybe Game
initGame int =
    Just <|
        { me = { id = int, position = 0.0, laser = Nothing }
        , keysDown = Set.empty
        }


initCable : ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable"
        |> ActionCable.withDebug True
        |> ActionCable.onWelcome (Just CableConnected)
        |> ActionCable.onConfirm (Just GameJoined)
        |> ActionCable.onDidReceiveData (Just DataReceived)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CableMsg msg_ ->
            ActionCable.update msg_ model.cable
                |> Tuple.mapFirst (\c -> { model | cable = c })

        ReceiveMyId (Ok id) ->
            ( { model | myId = Just id }, Cmd.none )

        ReceiveMyId (Err err) ->
            ( { model | error = Just "could not fetch id" }, Cmd.none )

        CableConnected _ ->
            joinGame model

        GameJoined _ ->
            -- TODO: send tick
            case model.myId of
                Just id ->
                    ( { model | game = initGame id }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Tick time ->
            let
                newGame =
                    Maybe.map (tick time) model.game
            in
                ( { model | game = newGame }, Cmd.none )

        KeyDown keyCode ->
            ( setInGame (\g -> { g | keysDown = Set.insert keyCode g.keysDown }) model, Cmd.none )

        KeyUp keyCode ->
            ( setInGame (\g -> { g | keysDown = Set.remove keyCode g.keysDown }) model, Cmd.none )

        DataReceived _ jsonValue ->
            ( dataReceived jsonValue model, Cmd.none )


dataReceived : JD.Value -> Model -> Model
dataReceived json model =
    Debug.log "data received" json
        |> decodeRemoteMsg
        |> Maybe.map (runRemoteMsg model)
        |> Maybe.withDefault model


decodeRemoteMsg : JD.Value -> Maybe RemoteMsg
decodeRemoteMsg value =
    let
        positionDecoder =
            JD.map2 Position
                (JD.field "x" JD.float)
                (JD.field "y" JD.float)

        playerDecoder =
            JD.map3 Player
                (JD.field "id" JD.int)
                (JD.field "y" JD.float)
                (JD.maybe <| JD.field "laser" positionDecoder)

        asteroidDecoder =
            JD.map2 Asteroid
                (JD.field "id" JD.int)
                positionDecoder
    in
        Result.toMaybe <|
            case JD.decodeValue (JD.field "msg" JD.string) value of
                Ok "asteroid_reset" ->
                    JD.decodeValue asteroidDecoder value
                        |> Result.map TheyReset

                Ok "player_moved" ->
                    JD.decodeValue playerDecoder value
                        |> Result.map TheyMoved

                Ok "collision" ->
                    JD.decodeValue (JD.map2 TheyCollided (JD.field "player_id" JD.int) (JD.field "asteroid_id" JD.int)) value

                Ok "destroy" ->
                    JD.decodeValue (JD.map2 TheyShot (JD.field "player_id" JD.int) (JD.field "asteroid_id" JD.int)) value

                _ ->
                    Err "message not known"


runRemoteMsg : Model -> RemoteMsg -> Model
runRemoteMsg model remoteMsg =
    case Debug.log "remote msg" remoteMsg of
        TheyCollided player asteroid ->
            model

        TheyMoved player ->
            model

        TheyReset asteroid ->
            model

        TheyShot player asteroid ->
            model


setInGame : (Game -> Game) -> Model -> Model
setInGame f model =
    case model.game of
        Nothing ->
            model

        Just g ->
            { model | game = Just <| f g }


fire : Model -> ( Model, Cmd Msg )
fire model =
    ( setInGame fireInGame model, Cmd.none )


fireInGame : Game -> Game
fireInGame game =
    game


tick : Time.Time -> Game -> Game
tick time game =
    game
        |> doMove


doMove : Game -> Game
doMove ({ me } as game) =
    let
        moveMe =
            if Set.member 38 game.keysDown then
                2.0
            else if Set.member 40 game.keysDown then
                -2.0
            else
                0.0
    in
        { game | me = { me | position = me.position + moveMe } }


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
gameView { game } =
    Element.toHtml <|
        C.collage 1000 500 <|
            case game of
                Nothing ->
                    noGame

                Just game ->
                    gameItems game


noGame : List C.Form
noGame =
    [ space
    , C.toForm <| Element.centered <| Text.color Color.lightGray <| Text.fromString "game not loaded"
    ]


gameItems : Game -> List C.Form
gameItems game =
    List.concat
        [ [ space ]
        , [ meView game.me ]
        ]


space : C.Form
space =
    C.filled Color.black <| C.rect 1000 500


meView : Player -> C.Form
meView me =
    C.square 20
        |> C.filled Color.white
        |> C.move ( -440, (250.0 / 100.0 * me.position) )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        cableListener =
            ActionCable.listen CableMsg model.cable
    in
        case model.game of
            Nothing ->
                cableListener

            Just g ->
                Sub.batch
                    [ cableListener
                    , AF.diffs Tick
                    , Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    ]
