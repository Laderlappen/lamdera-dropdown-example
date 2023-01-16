module Types exposing (..)

import Http exposing (..)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import W.Table exposing (bool)


type alias BackendModel =
    { counter : Int
    , wordList : List String
    }


type alias FrontendModel =
    { counter : Int
    , wordList : List String
    , selectedWord : String
    , clientId : String
    , loading : Bool
    }


type FrontendMsg
    = Increment
    | Decrement
    | FetchNewWords
    | FNoop
    | SelectWord String
    | FNoop3 Int


type ToBackend
    = CounterIncremented
    | CounterDecremented
    | FetchNewWordsClicked


type BackendMsg
    = ClientConnected SessionId ClientId
    | GotWordList ClientId (Result Http.Error WordType)
      --| GetWordsTask (Result Error (List WordType))
    | Noop


type ToFrontend
    = CounterNewValue Int String
    | SendWordListToFrontend (List String) String


type alias WordType =
    { words : List String }
