module Types exposing (..)

import Json.Decode exposing (..)

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



type alias Req = 
  { zip           : Int
  , city          : String
  , page          : Int
  , url           : String
  , distance      : Int
  , firma_pattern : FPattern
  , business      : Business
  , name          : String
  }

type FPattern = Contain 
              | BeginWith


type Business =  All
              | CarRent
              | Hotel
              | Food
              | Leasure
              | Travel
              | Gas
              | Other 
