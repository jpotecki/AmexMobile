module Model exposing (..)

import Table
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
  | SetTableState Table.State



type alias Model =
  { tableState  : Table.State
  , stores      : List Store
  , query       : String
  , errors      : List String
  }



init : List Store -> ( Model, Cmd Msg )
init stores =
  let model = { tableState  = Table.initialSort "Distance"
              , stores      = []
              , query       = ""
              , errors      = []
              }
  in  model ! []