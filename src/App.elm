module App exposing (..)

import Model exposing (..)
import View exposing (..)
import Types exposing (..)

import Html exposing (..)
import WebSocket
import Json.Decode exposing (..)
import Geocoding exposing (..)
import Geolocation
import Task exposing (..)
import APIKey exposing (key, wsAddress)



main : Program Never Model Msg
main =
  Html.program
    { init = init []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetQuery str ->
       { model | query = str } ! []

    Send ->
      model ! [WebSocket.send wsAddress model.query]

    NewMessage str -> 
      case decodeString decodeStore str of
        Ok s    -> 
          { model | stores = s :: model.stores } ! []
        Err err -> model ! []

    SetTableState state ->
      { model | tableState = state } ! []
    
    UsePosition ->
      model ! [ Geolocation.nowWith geoOpt |> Task.attempt GotLocation ] 
    GotLocation (Ok loc) ->
      let
        cmd = reverseRequestForLatLng key (loc.latitude, loc.longitude)
                |> Geocoding.withLocationTypes [ Rooftop ]
                |> Geocoding.withResultTypes geoResultTypes
                |> Geocoding.sendReverseRequest MyReverseGeocoderResult
      in
        model ! [cmd]
    GotLocation (Err err) ->
      { model | errors = (toString err) :: model.errors } ! []

    MyReverseGeocoderResult (Ok res) ->
      case List.head res.results of
        Nothing   -> { model | errors = "No Result" :: model.errors } ! []
        Just xs   ->
          let str = toString xs.formattedAddress
           in { model | errors = str :: model.errors } ! []

    MyReverseGeocoderResult (Err err) ->
      { model | errors = (toString err) :: model.errors } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsAddress NewMessage


geoResultTypes : List Geocoding.ComponentType
geoResultTypes = 
  [ Geocoding.StreetAddress
  , Geocoding.PostalCode
  -- , Geocoding.Country
  ]


geoOpt : Geolocation.Options
geoOpt =
  { enableHighAccuracy = True
  , timeout = Just 5000
  , maximumAge = Nothing
  }

-- {"zip":12627,"city":"Berlin","page":0,"url":"http://akzeptanz.amex-services.de/suche.php","distance":14,"firma_pattern":"BeginWi","business":"All","name":"Mc"}