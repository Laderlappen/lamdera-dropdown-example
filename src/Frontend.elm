module Frontend exposing (Model, app)

import ElmBook.Actions exposing (logAction, logActionWith)
import Html as H exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Lamdera exposing (sendToBackend)
import Types exposing (..)
import UI exposing (..)
import W.Button
import W.Container exposing (vertical)
import W.DataRow exposing (..)
import W.InputSelect
import W.Loading
import W.Message exposing (..)
import W.Styles


type alias Model =
    FrontendModel


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "v1"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { counter = 0, clientId = "", wordList = [ "None" ], selectedWord = "None", loading = False }, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, sendToBackend CounterDecremented )

        FetchNewWords ->
            ( { model | loading = True }, sendToBackend FetchNewWordsClicked )

        FNoop ->
            ( model, Cmd.none )

        SelectWord val ->
            ( { model | selectedWord = val }, Cmd.none )

        FNoop3 val ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = clientId }, Cmd.none )

        SendWordListToFrontend newWords clientId ->
            ( { model | wordList = List.append [ "None" ] newWords, loading = False, clientId = clientId }, Cmd.none )

        SendWordListFailedToFrontend newWords clientId ->
            ( { model | wordList = newWords, loading = False, clientId = clientId }, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    H.div [ style "padding" "30px" ]
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , UI.hSpacer
            [ W.Button.view
                []
                { label = [ text "+" ]
                , onClick = Increment
                }
            , W.DataRow.view [] [ H.text (String.fromInt model.counter) ]
            , W.Button.view
                []
                { label = [ text "-" ]
                , onClick = Decrement
                }
            ]
        , UI.vSpacer
            [ UI.hSpacer
                [ W.Button.view []
                    { label = [ text "Fetch new words" ]
                    , onClick = FetchNewWords
                    }
                , htmlIf (W.Loading.circles []) model.loading
                ]
            , W.InputSelect.view []
                { value = "One"
                , toLabel = toLabel
                , onInput = SelectWord
                , options = model.wordList
                }
            , W.DataRow.view []
                [ H.text (String.append "Selected Word: " model.selectedWord)
                ]
            , W.DataRow.view []
                [ H.text ""
                ]
            ]
        ]


toLabel =
    \x -> x


htmlIf : Html msg -> Bool -> Html msg
htmlIf el cond =
    if cond then
        el

    else
        text ""
