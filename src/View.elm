module View exposing (..)

import Table

import Model        exposing (..)
import Types        exposing (..)
import Html         exposing (..)
import Html.Events  exposing (..)

view : Model -> Html Msg
view { tableState, stores, query, errors } =
  div []
    [ h1 []  [ text "Amex Stores" ]
    , button [ onClick UsePosition] [ text "Geolocation" ]
    , input  [ onInput SetQuery ] []
    , button [ onClick Send] [ text "Send" ]
    , Table.view config tableState stores
    , div [] (printErr errors)
    ] 
  

config : Table.Config Store Msg
config = 
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
      [ Table.intColumn "km" .dist
      , Table.stringColumn "Name" .name
      , Table.stringColumn "address" .address
      ]
    }

printErr : List String -> List (Html Msg)
printErr err = List.map text err
          