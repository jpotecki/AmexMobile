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
import Tuple      exposing (first, second)
import Debug

view : Model -> Html Msg
view model =
  Material.Scheme.top <| Layout.render Mdl
        model.mdl
        [ Layout.selectedTab model.tab
        , Layout.onSelectTab SelectTab ]
        { header = [ text "Amex Mobile Search"]
        , drawer = []
        , tabs = ( [ text "home", text "map"], [ ] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    case model.tab of
        0 ->
            showHome model
        1 -> googleMap (googleAttribute model) []
        _ -> text "404"

googleAttribute : Model -> List (Html.Attribute Msg)
googleAttribute model =
  case model.pos of
    Just pos -> 
      [ Attribute.attribute "latitude"  (toString <| first pos)
      , Attribute.attribute "longitude" (toString <| second pos)
      , Attribute.attribute "drag-events" "true"
      , recordLatLongOnDrag
      ]
    Nothing -> Debug.log "Nothing" []



showHome : Model -> Html Msg
showHome model =
      -- div [] [ showLoading model.querying
      --        , Button.render Mdl [0] model.mdl
      --           [ Button.fab
      --           , Button.colored
      --           , Options.onClick UsePosition
      --           ] [ Icon.i "Get Data"]
      --        ]
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
              [ Options.styled p [ Typo.left ] [ showLink model store ] ]
          ]

  -- Table.table [] [ Table.thead []
  --                    [ Table.tr []
  --                      [ Table.th [ Table.numeric ] [ text "Distance" ]
  --                      , Table.th [] [ text "Name" ]
  --                      ]
  --                    ]
  --                , Table.tbody []
  --                (flip List.map (toList model.stores) (\store ->
  --                   Table.tr []
  --                     [ Table.td [] [ text <| toString store.dist ]
  --                     , Table.td [ Table.numeric ] [ text store.name ]
  --                     , Table.td [ ] [ (showLink model store) ]
  --                     ]
  --                   )
  --                )
  --                ]


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


-- view { stores, query, errors } =
--   case Dict.isEmpty stores of
--     True -> div [] [ button [ onClick UsePosition] [ text "Find near stores"] 
--                  , div [] (printErr errors) 
--                  ]
--     _  -> div []
--             [ h1 []  [ text "Amex Stores" ]
--             , button [ onClick UsePosition] [ text "Geolocation" ]
--             , input  [ onInput SetQuery ] []
--             , button [ onClick Send] [ text "Send" ]
--             -- , Table.view config tableState stores
--             , toList stores |> listStores 
--             , div [] (printErr errors)
--             ] 

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





