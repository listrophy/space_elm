module Main exposing (..)

import Html exposing (..)
import Html.Attributes as A exposing (href)
import Html.Events as E exposing (onInput, onClick)
import Http
import Json.Decode as JD
import Rails
import RemoteData exposing (RemoteData(..), WebData)
import ActionCable exposing (ActionCable)
import ActionCable.Msg as ACMsg
import ActionCable.Identifier as ID
import Collage as C
import Element
import Color
import Text
import Time
import AnimationFrame as AF
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias RemoteGame =
    { id : Int
    , name : String
    }


type alias Game =
    { metadata : RemoteGame
    , me : Ship
    , stars : List Star
    }


type alias Star =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Ship =
    { position : Float
    }


type alias Model =
    { games : WebData (List RemoteGame)
    , currentGame : Maybe Game
    , cable : ActionCable Msg
    , subscribedMessage : Maybe String
    }


type Msg
    = GamesRetrieved (Result Http.Error (List RemoteGame))
    | Tick Time.Time
    | ChooseGame String
    | CableMsg ACMsg.Msg
    | SubscriptionConfirmed ID.Identifier
    | InitializeStars (List ( Float, Float, Float ))


init : ( Model, Cmd Msg )
init =
    ( { games = NotAsked
      , currentGame = Nothing
      , cable = initCable
      , subscribedMessage = Nothing
      }
    , retrieveGames
    )


initGame : RemoteGame -> Game
initGame remoteGame =
    { metadata = remoteGame
    , me = { position = 0.0 }
    , stars = []
    }


retrieveGames : Cmd Msg
retrieveGames =
    JD.list gameDecoder
        |> Rails.get "/games"
        |> Http.send GamesRetrieved


initCable : ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable"
        |> ActionCable.withDebug True
        |> ActionCable.onConfirm (Just SubscriptionConfirmed)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GamesRetrieved r ->
            { model | games = RemoteData.fromResult r } ! []

        ChooseGame str ->
            case Result.toMaybe <| String.toInt str of
                Just int ->
                    if Maybe.withDefault False <| Maybe.map (gameHasId int) model.currentGame then
                        ( model, Cmd.none )
                    else
                        chooseGame int model

                Nothing ->
                    ( model, Cmd.none )

        CableMsg msg_ ->
            ActionCable.update msg_ model.cable
                |> Tuple.mapFirst (\c -> { model | cable = c })

        SubscriptionConfirmed id ->
            { model | subscribedMessage = Just <| toString id } ! []

        InitializeStars randList ->
            ( { model | currentGame = Maybe.map (setStars randList) model.currentGame }, Cmd.none )

        Tick time ->
            let
                newGame =
                    Maybe.map (tick time) model.currentGame
            in
                ( { model | currentGame = newGame }, Cmd.none )


setStars : List ( Float, Float, Float ) -> Game -> Game
setStars xyzs game =
    let
        setStar ( x, y, z ) =
            Star x y z
    in
        { game
            | stars = List.map setStar xyzs
        }


initializeStars : Cmd Msg
initializeStars =
    let
        f a b =
            Random.float a b
    in
        Random.generate InitializeStars (Random.list 20 <| Random.map3 (,,) (f -500 500) (f -250 250) (f 0.01 0.1))


tick : Time.Time -> Game -> Game
tick time game =
    { game
        | stars = List.map (tickStar time) game.stars
    }


tickStar : Time.Time -> Star -> Star
tickStar time star =
    let
        newX =
            star.x - time * star.z
    in
        if newX < -550 then
            { star | x = 550 }
        else
            { star | x = newX }


makeId : Int -> ID.Identifier
makeId int =
    ID.newIdentifier "GamesChannel" [ ( "id", toString int ) ]


chooseGame : Int -> Model -> ( Model, Cmd Msg )
chooseGame int model =
    let
        ( unsubModel, unsubCmd ) =
            unsubscribeFromGame int model
    in
        case subscribeToGame int unsubModel of
            Ok ( newModel, subCmd ) ->
                newModel ! [ unsubCmd, subCmd ]

            Err err ->
                { model | subscribedMessage = Just <| ActionCable.errorToString err } ! []


subscribeToGame : Int -> Model -> Result ActionCable.ActionCableError ( Model, Cmd Msg )
subscribeToGame int model =
    let
        updateModel cable =
            { model | cable = cable, currentGame = getGame int model.games }
    in
        model.cable
            |> ActionCable.subscribeTo (makeId int)
            |> Result.map (\( m, c ) -> ( updateModel m, Cmd.batch [ c, initializeStars ] ))


getGame : Int -> WebData (List RemoteGame) -> Maybe Game
getGame int =
    RemoteData.withDefault []
        >> listGet int
        >> Maybe.map initGame


unsubscribeFromGame : Int -> Model -> ( Model, Cmd Msg )
unsubscribeFromGame int model =
    (Result.fromMaybe ActionCable.ChannelNotSubscribedError model.currentGame)
        |> Result.andThen
            (\g -> ActionCable.unsubscribeFrom (makeId (gameId g)) model.cable)
        |> Result.map (Tuple.mapFirst (\cable -> { model | cable = cable }))
        |> Result.withDefault ( model, Cmd.none )


listGet : Int -> List RemoteGame -> Maybe RemoteGame
listGet id =
    List.filter (remoteGameHasId id)
        >> List.head


remoteGameHasId : Int -> RemoteGame -> Bool
remoteGameHasId int =
    (.id >> (==) int)


gameHasId : Int -> Game -> Bool
gameHasId int =
    (gameId >> (==) int)


gameId : Game -> Int
gameId =
    .metadata >> .id


gameDecoder : JD.Decoder RemoteGame
gameDecoder =
    JD.map2 RemoteGame
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)



--


view : Model -> Html Msg
view model =
    let
        flexGrow x =
            ( "flex-grow", toString x )
    in
        div [ A.style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "height", "100%" ) ] ]
            [ div [ A.style [ ( "display", "flex" ) ] ]
                [ strong [ A.style [ ( "padding-right", "10px" ), flexGrow 1 ] ] [ text "Space Elm!" ]
                , div [ A.style [ flexGrow 1, ( "text-align", "center" ) ] ]
                    [ text <| "Current game: "
                    , gamesView model
                    ]
                , div [ A.style [ flexGrow 1, ( "text-align", "right" ) ] ]
                    [ text <|
                        if model.subscribedMessage == Nothing then
                            "unsubscribed"
                        else
                            "subscribed!"
                    ]
                ]
            , div [ A.style [ ( "flex-grow", "2" ) ] ]
                [ gameView model ]
            ]


gamesView : Model -> Html Msg
gamesView model =
    let
        loading =
            [ option [ A.disabled True ] [ text "Not Loaded" ] ]

        opt i =
            option
                [ A.selected <| Maybe.withDefault False <| Maybe.map (gameHasId i.id) model.currentGame
                , A.value <| toString i.id
                ]
                [ text i.name ]
    in
        select [ onInput <| ChooseGame ] <|
            case model.games of
                Success list ->
                    option [ A.disabled True, A.selected (model.currentGame == Nothing) ] [ text "Select Game" ]
                        :: List.map opt list

                _ ->
                    loading


gameView : Model -> Html Msg
gameView { currentGame } =
    Element.toHtml <|
        C.collage 1000 500 <|
            case currentGame of
                Nothing ->
                    noGame

                Just game ->
                    gameItems game


noGame : List C.Form
noGame =
    [ space
    , C.toForm <| Element.centered <| Text.color Color.lightGray <| Text.fromString "no game selected"
    ]


gameItems : Game -> List C.Form
gameItems game =
    List.concat
        [ [ space ]
        , List.map starView game.stars
        ]


space : C.Form
space =
    C.filled Color.black <| C.rect 1000 500


starView : Star -> C.Form
starView star =
    C.circle 5
        |> C.filled Color.lightGray
        |> C.move ( star.x, star.y )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionCable.listen CableMsg model.cable
        , if model.currentGame == Nothing then
            Sub.none
          else
            AF.diffs Tick
        ]
