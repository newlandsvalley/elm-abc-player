module Notable ( Notable (..)
               , Performance
               , fromMelodyLine) where


import Performance exposing (..)
import Maybe exposing (map, withDefault)
import Debug exposing (..)

type alias AccumulatedTime = Float

{-| Note descriptions we need to keep-}
type Notable = Note Int Float

{-| Midi NoteEvent -}    
type alias NoteEvent = (AccumulatedTime, Notable)

{-| AbcPerformance -}    
type alias Performance = List NoteEvent

defaultGain = 1.0

fromNote : SingleNote -> Bool -> (AccumulatedTime, Maybe NoteEvent, Performance) -> (AccumulatedTime, Maybe NoteEvent, Performance)
fromNote n tied acc =
  let
    (t, mtie, p) = acc    
    event = (t, Note n.pitch defaultGain)
    nextTie = 
      if tied then 
        Just event
      else
        Nothing
  in
    if (n.pitch == 0) then
      (t + n.time, Nothing, p)
    else
      case mtie of
        Nothing ->    
           (t + n.time, nextTie, event :: p)
        Just (_, Note pitch g) ->
           if (n.pitch == pitch) then 
              (t + n.time, nextTie, p)
           else 
              (t + n.time, nextTie, event :: p)
      

fromChord : List SingleNote -> (AccumulatedTime, Maybe NoteEvent, Performance) -> (AccumulatedTime, Maybe NoteEvent, Performance)
fromChord ns acc =
  let
    (t, mtie, p) = acc
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
      -- we asssume we won't tie a chord
      (t + noteTime, Nothing, (List.append notes p))      

fromBar : ABar -> (AccumulatedTime,  Maybe NoteEvent, Performance) ->  (AccumulatedTime,  Maybe NoteEvent, Performance)
fromBar b acc =
  let   
    (t, mtie, p) = acc
    f ne acc =       
      case ne of
        ANote n tied ->
          fromNote (log "note" n) tied acc
        AChord c ->
          fromChord c acc
  in
    List.foldl f acc b.notes

fromMelodyLine : AccumulatedTime -> MelodyLine -> Performance
fromMelodyLine t m = 
  let
    (t, mtie, p) =  List.foldl fromBar (0.0, Nothing, []) m
  in
    p


