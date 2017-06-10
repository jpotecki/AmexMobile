module Model exposing (..)

import Types exposing (..)
import Geolocation exposing (..)
import Http
import Geocoding exposing (..)
import Material
import Dict

type Msg
  = SetQuery String
  | Send
  | UsePosition
  | GotLocation (Result Error Location)
  | MyReverseGeocoderResult (Result Http.Error Response)
  | NewMessage String
  | Mdl (Material.Msg Msg)



type alias Model =
  { stores      : Dict.Dict Distance ( List Store )
  , query       : Maybe Req
  , errors      : List String
  , mdl         : Material.Model
  }



init : ( Model, Cmd Msg )
init =
  let model = { stores      = Dict.empty
              , query       = Nothing
              , errors      = []
              , mdl         = Material.model
              }
  in  model ! []