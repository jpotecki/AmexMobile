module Model exposing (..)


import Types exposing (..)
import Geolocation exposing (..)
import Http
import Geocoding as Geo
import Material
import Dict
import Task exposing (..)
import Json.Decode as Decode exposing (..)
import Html.Events exposing (..)

import Html exposing (..)

type Msg
  = Send
  | UsePosition
  | GotLocation (Result Error Location)
  | MyReverseGeocoderResult Float Float (Result Http.Error Geo.Response)
  | NewMessage String
  | Mdl (Material.Msg Msg)



type alias Model =
  { stores      : Dict.Dict Distance ( List Store )
  , query       : Maybe Req
  , errors      : List String
  , mdl         : Material.Model
  , querying    : Bool
  }



init : ( Model, Cmd Msg )
init =
  let model = { stores      = Dict.empty
              , query       = Nothing
              , errors      = []
              , mdl         = Material.model
              , querying    = False
              }
  in  model ! []


quickPoll : ( Model, Cmd Msg )
quickPoll = 
  let model = { stores      = Dict.empty
              , query       = Nothing
              , errors      = []
              , mdl         = Material.model
              , querying    = True
              }
  in  model ! [ send UsePosition ]


send : Msg -> Cmd Msg
send msg =
  Task.succeed msg
  |> Task.perform identity


-- recordLatLongOnDrag : Attribute Msg
-- recordLatLongOnDrag =
--     on "map-moved" <|
--         Decode.map2 SetLatLong
--             (at [ "target", "latitude" ] float)
--             (at [ "target", "longitude" ] float)


onChange : (Float -> Msg) -> Attribute Msg
onChange toMsg =
    Decode.string
        |> Decode.andThen decodeLatLong
        |> Decode.at [ "target", "value" ]
        |> Decode.map toMsg
        |> on "change"


decodeLatLong : String -> Decoder Float
decodeLatLong str =
    case Decode.decodeString Decode.float str of
        Ok num ->
            Decode.succeed (num / 10000)

        Err err ->
            Decode.fail err