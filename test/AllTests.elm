module AllTests exposing (..)

import ElmTest exposing (..)

import Test.AbcPerformance as AbcPerformance
import Test.MidiPerformance as MidiPerformance

{- The code that generates a performance from parsed ABC text is not (yet) dry enough
   We have two modules:
     MidiPerformance (used by the Editor/Controller)
     AbcPerformance (used by the other examples)

   This is because the Editor Controller embeds the MIDI player control - i.e. it uses
   the MIDI format required by that player, whereas the earlier examples were written before
   the MIDI player was developed and simply play an uninterruptible stream of notes.

   However, the two performance types share a good deal of (repeated) source code.  I'm not sure where this will go
   - possibly eventually deprecated AbcPerformance and move all the other examples to the MIDI player.

-}


all : Test
all =
    suite "performance tests"
    [ 
      AbcPerformance.tests
    , MidiPerformance.tests
    ]

main : Program Never
main =
  runSuite all
