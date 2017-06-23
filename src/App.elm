module App exposing (..)

import Model exposing (..)
import View exposing (..)
import Types exposing (..)
import Html exposing (..)
import WebSocket
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Geocoding exposing (..)
import Geolocation
import Task exposing (..)
import APIKey exposing (key, wsAddress)
import Material




main : Program Never Model Msg
main =
  Html.program
    { init = quickPoll
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send ->
      case model.query of
        Nothing -> model ! []
        Just x ->
          let req = reqToJSON x
           in { model | querying = True} 
           ! [ WebSocket.send wsAddress (encode 0 req) ]

    NewMessage str -> 
      case decodeString decodeStore str of
        Ok s    -> 
          { model | stores = insert s model.stores} ! []
        
        Err err -> { model | errors = err :: model.errors } ! []

    UsePosition ->
      { model | querying = True }
      ! [ Geolocation.nowWith geoOpt |> Task.attempt GotLocation ] 

    GotLocation (Ok loc) ->
      let
        cmd = reverseRequestForLatLng key (loc.latitude, loc.longitude)
                |> Geocoding.withLocationTypes [ Rooftop ]
                |> Geocoding.withResultTypes geoResultTypes
                |> Geocoding.sendReverseRequest (MyReverseGeocoderResult loc.latitude loc.longitude)
      in
        model ! [ cmd ]

    GotLocation (Err err) ->
      { model | errors = (toString err) :: model.errors } ! []

    MyReverseGeocoderResult lat lon (Ok res) ->
      case getValid res lat lon of
        Nothing   -> { model | errors = "No Results" :: model.errors } ! []
        Just x    -> { model | query = Just x } ! [ Model.send Send ]

    MyReverseGeocoderResult _ _ (Err err) ->
      { model | errors = (toString err) :: model.errors } ! []

    Mdl (mdl) ->
      Material.update Mdl mdl model



updateQuery : Model -> Float -> Float -> Model
updateQuery model nlat nlon = 
  case model.query of 
    Nothing  -> model
    Just req -> 
      let new = { req | lat = nlat, lon = nlon} |> Just
       in { model | query = new }


subscriptions : Model -> Sub Msg
subscriptions model =
   WebSocket.listen wsAddress NewMessage



geoResultTypes : List Geocoding.ComponentType
geoResultTypes = 
  [ Geocoding.StreetAddress ]



geoOpt : Geolocation.Options
geoOpt =
  { enableHighAccuracy = True
  , timeout = Just 5000
  , maximumAge = Nothing
  }