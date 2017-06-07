module View exposing (..)


import Model        exposing (..)
import Types        exposing (..)
import Html         exposing (..)
import Html.Events  exposing (..)



view : Model -> Html Msg
view { stores, query, errors } =
  case stores of
    [] -> div [] [ button [ onClick UsePosition] [ text "Find near stores"] 
                 , div [] (printErr errors) 
                 ]
    _  -> div []
            [ h1 []  [ text "Amex Stores" ]
            , button [ onClick UsePosition] [ text "Geolocation" ]
            , input  [ onInput SetQuery ] []
            , button [ onClick Send] [ text "Send" ]
            -- , Table.view config tableState stores
            , listStores stores
            , div [] (printErr errors)
            ] 

listStores : List Store -> Html Msg
listStores s = 
  let 
    entries = List.map createEntry s
   in div [] entries


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





