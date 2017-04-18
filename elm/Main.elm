module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Rails
import RemoteData exposing (RemoteData(..), WebData)
import ActionCable exposing (ActionCable)
import ActionCable.Msg as ACMsg
import ActionCable.Identifier as ID


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Game =
    { id : Int
    , name : String
    }


type alias Model =
    { games : WebData (List Game)
    , currentGame : Maybe Game
    , cable : ActionCable Msg
    , subscribedMessage : Maybe String
    }


type Msg
    = GamesRetrieved (Result Http.Error (List Game))
    | ChooseGame Int
    | CableMsg ACMsg.Msg
    | SubscriptionConfirmed ID.Identifier


init : ( Model, Cmd Msg )
init =
    ( { games = NotAsked
      , currentGame = Nothing
      , cable = initCable
      , subscribedMessage = Nothing
      }
    , retrieveGames
    )


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

        ChooseGame int ->
            if Maybe.withDefault False <| Maybe.map (gameHasId int) model.currentGame then
                ( model, Cmd.none )
            else
                chooseGame int model

        CableMsg msg_ ->
            ActionCable.update msg_ model.cable
                |> Tuple.mapFirst (\c -> { model | cable = c })

        SubscriptionConfirmed id ->
            { model | subscribedMessage = Just <| toString id } ! []


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
            |> Result.map (Tuple.mapFirst updateModel)


getGame : Int -> WebData (List Game) -> Maybe Game
getGame int =
    RemoteData.withDefault []
        >> listGet int


unsubscribeFromGame : Int -> Model -> ( Model, Cmd Msg )
unsubscribeFromGame int model =
    (Result.fromMaybe ActionCable.ChannelNotSubscribedError model.currentGame)
        |> Result.andThen
            (\g -> ActionCable.unsubscribeFrom (makeId g.id) model.cable)
        |> Result.map (Tuple.mapFirst (\cable -> { model | cable = cable }))
        |> Result.withDefault ( model, Cmd.none )


listGet : Int -> List Game -> Maybe Game
listGet id =
    List.filter (gameHasId id)
        >> List.head


gameHasId : Int -> Game -> Bool
gameHasId int =
    (.id >> (==) int)


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.map2 Game
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)



--


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| "Current game: " ++ (Maybe.withDefault "(none)" <| Maybe.map .name model.currentGame) ]
        , div [] [ text <| "subscribed: " ++ (Maybe.withDefault "(none)" model.subscribedMessage) ]
        , gamesView model
        ]


gamesView : Model -> Html Msg
gamesView model =
    case model.games of
        NotAsked ->
            div [] [ text "Not asked" ]

        Loading ->
            div [] [ text "Loading" ]

        Failure e ->
            div [] [ text <| toString e ]

        Success list ->
            ul [] <|
                List.map (\i -> li [] [ a [ onClick <| ChooseGame i.id, href "#" ] [ text i.name ] ]) list


subscriptions : Model -> Sub Msg
subscriptions model =
    ActionCable.listen CableMsg model.cable
