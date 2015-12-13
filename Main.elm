module Main where

import CoordinateView exposing (..)
import Html exposing (Html)
import Coordinate exposing (..)
import Task exposing (Task, andThen, succeed, mapError)
import Http


model : Signal Model
model =
  Signal.foldp update init actions.signal


main : Signal Html
main =
  Signal.map view model


port request : Signal (Task Http.Error ())
port request =
    Signal.map getAddresses query.signal
      |> (Signal.map (\task -> Task.toResult task `andThen` (UpdateAddresses >> Signal.send actions.address)))
