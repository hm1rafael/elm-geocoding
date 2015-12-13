module CoordinateView where

import Html exposing (Html, text, div, input, button)
import Html.Attributes exposing (type', placeholder)
import String
import Html.Events exposing (on, targetValue, onClick)
import Coordinate exposing (..)


div' : String -> Html
div' address =
  div [][text <| address]


view : Model -> Html
view model =
  let
    addressElement =
      case model.addresses of
        Err msg ->
          div [] [text msg]
        Ok addresses ->
          div [] (List.map div' addresses)
    div' address =
      div [][text <| address]
  in
    div []
    [ input
    [ type' "String"
    , placeholder "latitude"
    , on "input" targetValue (\latitude -> Signal.message actions.address (SetLatitude latitude))
    ][]
    , input
    [ type' "String"
    , placeholder "longitude"
    , on "input" targetValue (\longitude -> Signal.message actions.address (SetLongitude longitude))
    ][]
    , button [onClick query.address model][text "Search"]
    , addressElement
    ]


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


query : Signal.Mailbox Coordinate
query =
  Signal.mailbox init
