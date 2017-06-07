module Model exposing (..)

import Types exposing (..)
import Geolocation exposing (..)
import Http
import Geocoding exposing (..)



type Msg
  = SetQuery String
  | Send
  | UsePosition
  | GotLocation (Result Error Location)
  | MyReverseGeocoderResult (Result Http.Error Response)
  | NewMessage String



type alias Model =
  { stores      : List Store
  , query       : Maybe Req
  , errors      : List String
  }



init : List Store -> ( Model, Cmd Msg )
init stores =
  let model = { stores      = []
              , query       = Nothing
              , errors      = []
              }
  in  model ! []