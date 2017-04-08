module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Rails
import RemoteData exposing (RemoteData(..), WebData)


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
    }


type Msg
    = RetrieveGames
    | GamesRetrieved (Result Http.Error (List Game))
    | ChooseGame Int


init : ( Model, Cmd Msg )
init =
    ( { games = NotAsked
      , currentGame = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrieveGames ->
            ( model
            , JD.list gameDecoder
                |> Rails.get "/games"
                |> Http.send GamesRetrieved
            )

        GamesRetrieved r ->
            case r of
                Ok gameList ->
                    { model | games = Success gameList } ! []

                Err err ->
                    { model | games = Failure err } ! []

        ChooseGame int ->
            { model | currentGame = listGet int <| RemoteData.withDefault [] model.games } ! []


listGet : Int -> List Game -> Maybe Game
listGet id =
    List.filter (.id >> (==) id)
        >> List.head


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.map2 Game
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)



--


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick RetrieveGames ] [ text "Fetch" ] ]
        , div [] [ text <| "Current game: " ++ (Maybe.withDefault "(none)" <| Maybe.map .name model.currentGame) ]
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
    Sub.none
