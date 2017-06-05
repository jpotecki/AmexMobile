module App exposing (..)

import Model exposing (..)
import View exposing (..)
import Types exposing (..)

import Html exposing (..)
import WebSocket
import Json.Decode exposing (..)
import Table
import Geocoding exposing (..)
import Geolocation
import Task exposing (..)
import APIKey exposing (key)

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
      model ! [WebSocket.send "ws://localhost:8080" model.query]

    NewMessage str -> 
      case decodeString decodeStore str of
        Ok s    -> 
          { model | stores = s :: model.stores } ! []
        Err err -> model ! []

    SetTableState state ->
      { model | tableState = state } ! []
    
    UsePosition ->
      let
        cmd = Geolocation.nowWith geoOpt |> Task.attempt GotLocation
      in
        model ! [ cmd ] 
    GotLocation (Ok loc) ->
      let
        geo = (loc.latitude, loc.longitude)
        cmd = reverseRequestForLatLng key geo
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
        { model | errors = (toString xs.formattedAddress) :: model.errors } ! []

    MyReverseGeocoderResult (Err err) ->
      { model | errors = (toString err) :: model.errors } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:8080" NewMessage


geoResultTypes : List Geocoding.ComponentType
geoResultTypes = 
  [ Geocoding.StreetAddress
  , Geocoding.PostalCode
  -- , Geocoding.Country
  ]

geoOpt : Geolocation.Options
geoOpt =
  { enableHighAccuracy = True
  , timeout = Just 20000
  , maximumAge = Nothing
  }

-- {"zip":12627,"city":"Berlin","page":0,"url":"http://akzeptanz.amex-services.de/suche.php","distance":14,"firma_pattern":"BeginWi","business":"All","name":"Mc"}