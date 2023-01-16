module Backend exposing (app, init)

import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, list, string)
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { counter = 0, wordList = [] }, Cmd.none )


wordListDecoder : Decode.Decoder WordType
wordListDecoder =
    Decode.map WordType
        (Decode.field "words" <| Decode.list Decode.string)


fetchWordList : ClientId -> Cmd BackendMsg
fetchWordList clientId =
    Http.get
        { url = "http://shelled-psychedelic-honeycrisp.glitch.me/words"
        , expect = Http.expectJson (GotWordList clientId) wordListDecoder
        }


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model, sendToFrontend clientId <| CounterNewValue model.counter clientId )

        GotWordList clientId (Ok wordList) ->
            ( { model | wordList = List.append [ "None" ] wordList.words }, sendToFrontend clientId (SendWordListToFrontend wordList.words clientId) )

        GotWordList clientId (Err _) ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CounterIncremented ->
            let
                newCounter =
                    model.counter + 1
            in
            ( { model | counter = newCounter }, broadcast (CounterNewValue newCounter clientId) )

        CounterDecremented ->
            let
                newCounter =
                    model.counter - 1
            in
            ( { model | counter = newCounter }, broadcast (CounterNewValue newCounter clientId) )

        FetchNewWordsClicked ->
            ( model, fetchWordList clientId )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]
