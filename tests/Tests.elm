module Tests exposing (..)

import Test exposing (..)
import Test.AbcPerformance exposing (tests)
import Test.MidiPerformance exposing (tests)


all : Test
all =
  concat
    [ 
      Test.AbcPerformance.tests
    , Test.MidiPerformance.tests
    ]
