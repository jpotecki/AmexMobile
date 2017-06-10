module View exposing (..)


import Model        exposing (..)
import Types        exposing (..)
import Html         exposing (..)
import Html.Events  exposing (..)
import Dict
import Dict         exposing (Dict)




view : Model -> Html Msg
view { stores, query, errors } =
  case Dict.isEmpty stores of
    True -> div [] [ button [ onClick UsePosition] [ text "Find near stores"] 
                 , div [] (printErr errors) 
                 ]
    _  -> div []
            [ h1 []  [ text "Amex Stores" ]
            , button [ onClick UsePosition] [ text "Geolocation" ]
            , input  [ onInput SetQuery ] []
            , button [ onClick Send] [ text "Send" ]
            -- , Table.view config tableState stores
            , toList stores |> listStores 
            , div [] (printErr errors)
            ] 

listStores : List Store -> Html Msg
listStores s = div [] <| List.map createEntry s


toList : Dict Distance (List Store) -> List Store
toList stores =
  Dict.toList stores |> List.map (\(_,x) -> x ) |> List.concat



createEntry : Store -> Html Msg
createEntry store =
  showStore store |> List.singleton |> div []



showStore : Store -> Html Msg
showStore store = 
  div [] [ span [] [ text <| toString store.dist ] 
         , span [] [ text store.name ]
         ]



printErr : List String -> List (Html Msg)
printErr err = List.map text err





