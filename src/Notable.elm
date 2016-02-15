module Notable ( Notable (..)
               , Performance
               , fromMelodyLine) where


import AbcPerformance exposing (..)
import Maybe exposing (map, withDefault)

type alias AccumulatedTime = Float

{-| Note descriptions we need to keep-}
type Notable = Note Int Float

{-| Midi NoteEvent -}    
type alias NoteEvent = (AccumulatedTime, Notable)

{-| AbcPerformance -}    
type alias Performance = List NoteEvent

defaultGain = 1.0

fromNote : SingleNote -> (AccumulatedTime, Performance) -> (AccumulatedTime, Performance)
fromNote n acc =
  let
    (t, p) = acc
  in
    if (n.pitch == 0) then
      (t + n.time, p)
    else
      let
         event = (t, Note n.pitch defaultGain)
      in
         (t + n.time, event :: p)

fromChord : List SingleNote -> (AccumulatedTime, Performance) -> (AccumulatedTime, Performance)
fromChord ns acc =
  let
    (t, p) = acc
  in
    let
      f n = (t, Note n.pitch defaultGain)
      notes = List.map f ns
      -- increment the time just by that of the first note in the chord
      -- all the others should be the same
      noteTime =  List.head ns
        |> Maybe.map (\n -> n.time)
        |> withDefault 0.0 
    in
      (t + noteTime, (List.append notes p))      

fromBar : ABar -> (AccumulatedTime, Performance) ->  (AccumulatedTime, Performance)
fromBar b acc =
  let   
    (t, p) = acc
    f ne acc =       
      case ne of
        ANote n  ->
          fromNote n acc
        AChord c ->
          fromChord c acc
  in
    List.foldl f acc b.notes

fromMelodyLine : AccumulatedTime -> MelodyLine -> Performance
fromMelodyLine t m = 
  List.foldl fromBar (0.0, []) m
    |> snd


