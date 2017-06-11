module Types exposing (..)

import Json.Decode  exposing (..)
import Json.Encode
import Geocoding    exposing (..)
import List         exposing (..)
import Maybe
import Maybe.Extra  exposing (..)
import Result
import String
import Material
import Dict
import Dict         exposing (Dict)
import Html         exposing (Html, Attribute)

type alias Store =
  { name : String
  , phone : String
  , zip : Int
  , city : String
  , address : String
  , dist : Int}


decodeStore : Decoder Store
decodeStore = 
  map6 Store
    (at ["name"] string)
    (at ["phone"] string)
    (at ["zip"] int)
    (at ["city"] string)
    (at ["address"] string)
    (at ["dist"] int)



extractFrom :  List ComponentType -> List AddressComponent -> Maybe String
extractFrom c a = 
  mapTillSucc a (\x -> if x.types == c then x.longName else Nothing)



{-
  iterates over a list till the first Just b occurs
-}
mapTillSucc : List a -> (a -> Maybe b) -> Maybe b
mapTillSucc xs f =
  case head xs |> Maybe.andThen f of
    Just x  -> Just x
    Nothing -> tail xs |> Maybe.andThen (flip mapTillSucc f)




getValid : Response -> Maybe Req
getValid res = mapTillSucc res.results tryTransform




tryTransform : GeocodingResult -> Maybe Req
tryTransform res =
  let
      zip_code = extractFrom [ PostalCode ]           res.addressComponents
      city     = extractFrom [ Locality, Political ]  res.addressComponents
      street   = extractFrom [ Route ]                res.addressComponents
      streetNo = extractFrom [ StreetNumber ]         res.addressComponents
      country  = extractFrom [ Country, Political ]   res.addressComponents
  in 
    -- could use map5 here, but this is better to expand/change
    mapM Req (toInt_ zip_code)
      |> andMap city
      |> andMap country
      |> andMap (Just 5)          -- distance
      |> andMap (Just Contain)
      |> andMap (Just Food)
      |> andMap (Just "")         -- name


type alias Req =
  { zip           : Int
  , city          : String
  , country       : String
  , distance      : Int
  , firma_pattern : FPattern
  , business      : Business
  , name          : String
  }


toInt_ : Maybe String -> Maybe Int
toInt_ str = mapM String.toInt str |> Maybe.andThen Result.toMaybe

mapM : (a -> b) -> Maybe a -> Maybe b
mapM = Maybe.map



reqToJSON : Req -> Value
reqToJSON req =
  Json.Encode.object  
    [ ( "zip", Json.Encode.int req.zip )
    , ( "city", Json.Encode.string req.city )
    , ( "country", Json.Encode.string req.country )
    , ( "distance", Json.Encode.int req.distance)
    , ( "firma_pattern", fpToJSON req.firma_pattern)
    , ( "business", busToJSON req.business)
    , ( "name", Json.Encode.string req.name)
    ]

type FPattern = Contain 
              | BeginWith


fpToJSON : FPattern -> Value
fpToJSON x = 
  case x of 
    Contain   -> Json.Encode.string "Contain"
    BeginWith -> Json.Encode.string "BeginWi"

type Business = All
              | CarRent
              | Hotel
              | Food
              | Leasure
              | Travel
              | Gas
              | Other 

busToJSON : Business -> Value
busToJSON x =
  case x of
    All     -> Json.Encode.string "All"
    CarRent -> Json.Encode.string "CarRent"
    Hotel   -> Json.Encode.string "Hotel"
    Food    -> Json.Encode.string "Food"
    Leasure -> Json.Encode.string "Leasure"
    Travel  -> Json.Encode.string "Travel"
    Gas     -> Json.Encode.string "Gas"
    Other   -> Json.Encode.string "Other"



type alias Mdl =
  Material.Model


type alias Distance = Int



type alias Stores = Dict Distance (List Store)

insert : Store -> Dict Distance (List Store) -> Dict Distance (List Store)
insert new dict =
  case Dict.member new.dist dict of
    True -> Dict.update new.dist (\xs -> addStore xs new) dict
    _    -> Dict.insert new.dist [ new ] dict

addStore : Maybe (List Store) -> Store -> Maybe (List Store)
addStore list store = 
  Maybe.map (\xs -> store :: xs) list |> Maybe.map (sortBy .name)

-- currently not possible, as Store doesn't save contain business area
-- filterStores : List Business -> Stores -> Stores
-- filterStores bs xs =
--   flip Dict.map xs
--     <| \_ x -> flip List.filter x
--     <| \y -> List.foldl ( || ) False
--     <| \z -> List.map (\b -> List.member b z.business) bs

googleMap : List (Attribute a) -> List (Html a) -> Html a
googleMap =
    Html.node "map-wrapper"