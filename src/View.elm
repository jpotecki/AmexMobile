module View exposing (..)

import Model        exposing (..)
import Types        exposing (..)
import Html         exposing (..)
-- import Html.Events  exposing (..)
import Html.Attributes as Attribute
import Dict
import Dict         exposing (Dict)
import Material.Scheme
import Material.Layout as Layout
-- import Html.Attributes exposing (href, class, style)
import Material.Button as Button
-- import Material.Table  as Table
import Material.Options as Options
import Material.Icon as Icon
import Material.Progress as Loading
import Material.List as List
import Material.Typography as Typo
import String


view : Model -> Html Msg
view model =
  Material.Scheme.top <| Layout.render Mdl
        model.mdl
        []
        { header = [ text "Amex Mobile Search"]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ showHome model ]
        }



showHome : Model -> Html Msg
showHome model =
    div [] [ showLoading model.querying
           , drawStores model
           ]


drawStores : Model -> Html Msg
drawStores model =
  let
      storename = \x -> String.toLower x |> String.trim |> text
  in
    List.ul [] 
    <| flip List.map (toList model.stores)
    <| \store -> 
        List.li [] 
          [ List.content [] 
              [ Options.styled p [ Typo.capitalize ] [ storename store.name ] ]
          , List.content [] 
              [ Options.styled p []                  [ text <| toString store.dist ] ]
          , List.content []
              [ Options.styled p [ Typo.left ]       [ showLink model store ] ]
          ]



showLink : Model -> Store -> Html Msg
showLink model store =
  Button.render Mdl [0] model.mdl
  [ Button.minifab
  , Button.colored
  , Button.link <| googleLink store
  , Options.attribute <| Attribute.target "_blank"
  ]
  [ Icon.i "zoom_in" ]

googleLink : Store -> String
googleLink store =
  let 
    str = String.join "," [store.name, store.address, toString store.zip, store.city]
   in String.concat [ "https://www.google.com/maps/dir/Current+Location/" , str ]

showLoading : Bool -> Html Msg
showLoading = \x ->
  if x then Loading.indeterminate else span [] []



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





