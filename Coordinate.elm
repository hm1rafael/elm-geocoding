module Coordinate where


import String
import Json.Decode exposing ((:=), at)
import Task exposing (Task, andThen, succeed, mapError)
import Http


type alias Coordinate =
  { latitude : String
  , longitude : String
  , addresses : Result String (List String)
  }


type alias Model =
  Coordinate


init : Model
init =
  { latitude = ""
  , longitude = ""
  , addresses = (Ok [])
  }


type Action =
  NoOp | UpdateAddresses (Result String (List String))
  | SetLatitude String | SetLongitude String


update : Action -> Model -> Model
update action model =
  let
    latitude =
      Debug.log "latitude" model.latitude -- old value debug
    longitude =
      Debug.log "longitude" model.longitude
    addresses =
      Debug.log "addresses" model.addresses
  in
    case action of
      NoOp ->
        model
      UpdateAddresses addressResult ->
        let
          adjustedAddressResult =
            case addressResult of
              Ok listAddresses ->
                if List.isEmpty listAddresses && not (String.isEmpty model.latitude) && not (String.isEmpty model.longitude)
                then (Err "No results found for the coordinates!!!")
                else if String.isEmpty model.latitude && String.isEmpty model.longitude
                then (Err "Give me a coordinate (Latitude/Longitude)!!!")
                else if String.isEmpty model.latitude
                then (Err "Give me a latitude!!!")
                else if String.isEmpty model.longitude
                then (Err "Give me a longitude!!!")
                else addressResult
              Err str ->
                addressResult
        in
          { model | addresses = adjustedAddressResult }
      SetLatitude latitude ->
        { model | latitude = latitude }
      SetLongitude longitude ->
        { model | longitude = longitude }


address : Json.Decode.Decoder String
address =
  Json.Decode.object1 toString
    ("formatted_address" := Json.Decode.string)


getAddresses : Model -> Task String (List String)
getAddresses model =
  let url =
    succeed(
      String.concat
        [ "https://maps.googleapis.com/maps/api/geocode/json?latlng="
        , model.latitude
        , ","
        , model.longitude
        , "&key=AIzaSyD1fgtZPKk5gdcy6MZjVLySMAQlNiatO1g"
        ]
    )
  in
    url `andThen` (mapError (always "Oops! A problem just happened") << Http.get (at["results"] (Json.Decode.list address)))
