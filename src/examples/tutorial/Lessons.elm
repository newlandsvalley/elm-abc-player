module Lessons
  ( Lesson
  , lessons
  ) where

{-|  Lessons in learning ABC

# Definition

# Data Types
@docs Lesson


# Functions
@docs lessons

-}

import Array exposing (Array, fromList)

type alias Lesson = 
  { title : String
  , instruction : String
  , example : String
  , hint : String
  }

instNotes = 
   "Use the characters A-G for the notes of the octave starting from middle C and a-g for the octave above." ++
   " You can place notes next to each other or separate them with spaces - it won't make much difference to " ++
   " the sound but neighbouring notes will be 'beamed' together in a score."

xmplNotes = 
  "A B c def"

hintNotes =
  "Try altering some of the notes."

instBarsAndRests = 
  "Use the character z to represent a rest.  You can set the length of a rest in exactly the same manner as for a note - by adding a number after it" ++
  " - for example z4. Use a vertical bar to introduce a bar line. You can spread out into multiple lines if you like."

xmplBarsAndRests =
  "| ABc z z def |\r\n" ++
  "| g z z z a |"

hintBarsAndRests =
  "Try adding another bar which contains both notes and rests."

instOctaves =
  "You can reach octaves below middle C by adding one (or more) commas immediately after the note." ++
  " Similarly higher octaves can be reached using apostrophes."

xmplOctaves = 
  "| C,, G,, C, G, | C G c g | c' g' c'' |"

instLongNotes =
  "So far, all our notes have had a duration of 1 unit.  You can extend this by placing a whole number" ++
  " after the note (and after the octave marker if you have one)."

xmplLongNotes =
  "| F G A B c4 g2 b2 c'4 |"

hintLongNotes =
  "Try making some of these notes even longer."

instShortNotes =
  "You can shorten a note by placing a fraction after the note.  This could be, for example," ++
  " 1/2 or 1/3. A shorthand for 1/2 is simply / and a shorthand for 1/3 is simply /3." ++
  " You can extend rests the same way."

xmplShortNotes =
  "| C3/2 G1/2 E3/2 G1/2 C3/2 G/ E3/2 G/ |"

instHornpipe =
  "The last example was in a hornpipe-like rhythm.  Because this is so common, there is a shorthand for it" ++
  " where you separate each pair of notes with the > character.  This extends the first note by half its length" ++
  " and reduces the second by the same amount."

xmplHornpipe =
  "| C>GE>G C>GE>G | c>de>d c>BA>G |"

hintHornpipe =
  "If you know it, can you finish off the 'A' part of the tune?"

instStrathspey =
  "Conversely, you can shorten the first note of a pair and lengthen the second by means of the < character." ++
  " This rhythm is found in strathspeys." 

xmplStrathspey =
  "| G | c2 e>c G<c e>g | c'2 b>c' a<c' g>e |"

instChords = 
   "You can play a chord by placing a group of notes, beamed together, inside square brackets - for example [CEa]." 

xmplChords = 
  "A2 Bc dcBc [CEa]"

instKeySig = 
   "So far, we have only used the white notes on the piano - i.e. the tune snippets have tended to be in the keys either" ++
   " of C Major or A Minor.  We now introduce our first header - K: for Key Signature.  Headers are placed on lines on their own" ++
   " before the melody.  In this way, we can move the last example from A Minor to A Major. This, of course, has the effect of" ++
   " sharpening every C,F and G." 

xmplKeySig = 
  "K: AMajor \r\n| A2 Bc dcBc [CEa] |"

instFlatKeySig =
  "If your key is a major key, you can, if you want, leave out the word 'Major'.  If it is a flat key, you use 'b' and if a sharp key, '#'. " ++
  " You can also choose to shorted the mode name to just three letters - in this case, BbMaj."
  

xmplFlatKeySig = 
   "K: Bb\r\n| BfdB AecA | FdBF D4 |"

instNaturals = 
   "If your key means that certain notes are sharpened or flattened, but you need to play the 'natural' " ++
   " (unsharpened or unflattened) note, then you can override the key by using an equals symbol immediately before the note." ++
   " Remember that, as in a score, you only need to mark as natural the first occurrence of the note in any given bar." ++
   " For example, this reverts the previous tune to a minor feel although the key is still a major one. Each C is natural."

xmplNaturals = 
  "K: AMajor \r\n| A2 B=c dcBc [CEa] |"

instAccidentals =
   "Similarly, you can sharpen a note by placing a caret symbol (^) immediately before it and flatten it using an underscore" ++
   " symbol (_). If you need a double sharp or double flat, then just double the appropriate symbol." ++
   " This example reverts the major feel although the key is now A Minor. Each C is sharpened."

xmplAccidentals =
 "K: AMinor \r\n| A2 B^c dcBc [CEa] |"

instUnitNote =
   "You may have noticed when we first introduced notes that we talked about their duration in 'units'.  But how long is a unit?" ++
   " So far, we have used the convention that it represents an eighth note (a quaver).  In other words, in a score, this is how" ++
   " The note would look.  We can change the unit to be a sixteenth note (a semiquaver) if we use the L (unit note length) header" ++
   " This will have the effect of doubling the speed."

xmplUnitNote =
 "L: 1/16 \r\nA B c def"

instTempo =
   "An accurate tempo is defined by means of the Q (tempo) header.  Up till now, we've used a default where we have 120 quarter notes per minute" ++
   " i.e 1/4=120.  We can, for example, slow down our tune firstly by reverting to a unit note length of 1/8 and secondly by explicitly reducing the " ++
   " tempo with the Q header."

xmplTempo =
  "L: 1/8 \r\nQ: 1/4=60\r\nA B c def"
  
instMeter =
  "The meter is defined with the M header.  For example, a waltz would normally have the meter 3/4 and a march 4/4." ++
  " 3/4 means that each complete bar should have a total duration equal to that of three quarter notes." ++
  " The presence of a meter actually makes little difference to how the tune sounds, but will show up in a score." ++
  " But it is important to make sure that the duration of each complete bar agrees with the meter you designate." ++
  " This example is a slip-jig in 9/8"

xmplMeter =
  "X:1\r\nT:Another jig will do\r\nQ:3/8=120\r\nM:9/8\r\nK:D\r\n" ++
  "ABA A2G F2G | ABA AGF G2E |\r\n" ++
  "ABA A2G F2G | A2d d2c d3 |\r\n" ++
  "A2g f2d e2c | A2B =c2B c2B |\r\n" ++
  "A2g f2d e2^c | A2d d2c d3 |\r\n" ++
  "A2g f2d e2c | A2B =c2B c2^c |\r\n" ++
  "d2A A2G F2G | A2d d2c d3 |\r\n"

instTie = 
  "A tie joins together two notes of the same pitch.  It is indicated by placing a hyphen directly after the first note of the pair." ++
  " The second note may follow immediately, but it can be separated by spaces or even a bar line.  The effect is to play one long note" ++
  " with the combined duration of the pair.  If the notes are of different pitches, the tie will simply be ignored." 

xmplTie = 
   "| G2 | c2c2 A2Ac | B2B2- B2AB |"

instTriplet = 
   "A triplet is usually used if you want to play three notes in the time normally taken by two."  ++
   " You introduce three notes of the same length placed together with the symbol (3" ++
   " This is extremely common in Swedish polskas - for example the start of the Grind Hans Jässpôdspolska."

xmplTriplet = 
   "K:Dmaj\r\n| A2 d2 e>f | (3g2f2d2 B2- |"

instComplexTriplet = 
   "If your triplet has notes of different lengths, you have to use the complex triplet notation." ++
   " For example (3:2:4d2d2Bd means play the rhythm of three notes in the time of two over the following group" ++
   " of four notes."

xmplComplexTriplet = 
   "K:Gmaj\r\n| D2 G>A B>c| (3:2:4d2d2Bd g2|"

instQuadruplet = 
   "Quadruplets are used if you want to play four notes in the time usually taken by three." ++
   " In a similar fashion to triplets, introduce four notes of the same length placed together" ++
   " with the symbol (4. This example contains triplets, a tie and a quadruplet."

xmplQuadruplet = 
   "K:Amaj\r\n| (3efg a2 a>b | (3agf e2-e>e | (4f2d2e2c2 | d>f (3f2e2c2 |"

instRepeat =
   "You can indicate that a section should be repeated by sandwiching it between bars which use the colon as a repeat marker - |: and :|" ++
   " The initial repeat marker at the first bar is optional."

xmplRepeat =
  "| C2 D2 E2 C2 :|: E2 F2 G4 :|\r\n|: GAGF E2 C2 :|: C2 G,2 C4 :|"

instRepeatVariants =
  "In some tunes, the two repeats may differ in their endings.  You can indicate that using |1 and |2 for the two variant endings"

xmplRepeatVariants =
  "L: 1/16\r\nK:Dmaj\r\n|: A4 a4 a2f2 | gfga b3a g2f2 | e3f g2b2 a2g2 | f3e d2c2 d2B2 |\r\n" ++
  "|1 B2A^G A8 :|2 B2AG F2EF A2A,2 | A,2D2 D8 |"

instTitle =
   "Very many of our previous examples have had no headers - only the melody line.  But, in fact a legitimate ABC tune always" ++
   " requires some headers.  The first is largely irrelevant - a reference number denoted by X.  Any number will do" ++
   " in most cases. The second header must be the tune title - T. You should also include the L (note length) and  M (meter) headers" ++
   " introduced earlier. Finally, the K (key) header should always be the last one."

xmplTitle =
  "X:1\r\nT:Camptown Races\r\nM:4/4\r\nL:1/8\r\nK:D\r\n|AAFA|BAF2|FE2z|FE2z|AAFA|BAF2|E2FE|D2-D2|\r\n|D>DFA|d4|B>BdB|A3F|\r\nAA F/2F/2 A/2A/2|BAF2|EF/2-G/2FE|D4 |\r\n"

instRhythm =
  "You can use the R (rhythm) header to indicate the type of tune (jig, reel, polska etc.). In most ABC collections, this field is optional." ++
  " However, if you want to save your tune to tradtunedb, it requires a rhythm header to be present so that you can search" ++
  " easily for tunes of the same type" 

xmplRhythm =
  "X: 1\r\nT: Kapten Lindholms Engelska\r\nR: engelska\r\nM: 4/4\r\nL: 1/8\r\nK:Amaj\r\n" ++
  "|: ed | cAce dcdf | ecAF E2 ed | cABc defg | aece agfe | cAce dcdf | ecAF E2 ed | cABc defg | a2 ag a2 :|\r\n" ++
  "|: e2 | aac'e aac'e | bbd'e bbd'e | aac'e aac'e | efed cB A2| fdfa ecea | fdfa ecea |fdfa gegb | baag a2 :|\r\n"

instInformation =
  "There are various other headers that you can use to add information about the tune as free text.  The most important are these: " ++
  " C (composer), O (geographical origin), S (source - where or how the tune was collected) and Z (the tune transcriber)."

xmplInformation =
  "X: 1\r\nT: Gubbdansen\r\nS: from 12 låtar för 2 eller 3 fioler med Gärdebylåten i Hjort Anders Olssons originalsättning\r\n" ++
  "Z: John Batchellor\r\nR: polska\r\nM: 3/4\r\nL: 1/16\r\nK:Dmin\r\n" ++
  "|: f3g f4 a4 | a2ba g2ag f2e2 | d3e f2g2 a2f2 | f3e e2^c2 A4 :|\r\n" ++
  "|: ^c2c2 d2d2 e2e2 | f2f2 gfed e4 | ^c2c2 d2d2 e2e2 | f2f2 gfed e4 |\r\n" ++
  "a4 b2a2 g2f2 | f2ef g2f2 e2d2 | fed^c c4 d4 :|\r\n"

instChangeKey =
  "If a tune changes key, you can indicate this simply by placing the K (key) header inside the score at the point where the key changes." ++
  " In this example, the first part of the tune is in B Minor and the second part in F# Minor." ++
  " Various other headers can be used within the score in this way - in particular, the M (meter) and L (unit note length) headers."

xmplChangeKey =
  "T:Polska från Småland \r\nM:3/4\r\nL:1/16\r\nR:polska\r\nK:Bmin\r\n" ++
  "|: B4 A4 B4 | d2f2 e2dc c2d2 | B2B2 A2A2 B2B2 |d2f2 e2dc d4 |\r\n" ++
  "F2GA B2AB c2Bc |d2cd edcB A2F2 | F2GA B2AB c2Bc |d2cd edcB A2F2 |\r\n" ++
  "F2GA B2c2 d3B | B2A2 B8 :|\r\n" ++
  "K:F#Min\r\n" ++
  "|: f4 e4 f4 |g2a2 b2ag g2a2 |f2f2 e2e2 f2f2 |g2a2 b2ag a4 |\r\n" ++
  "c2de f2ef g2fg |a2ga bagf e2c2 | c2de f2ef g2fg |a2ga bagf e2c2 |\r\n" ++
  "c2de f2g2 a3f |f2e2 f8 :|\r\n"
  
instMixolydian =
  "If you come across a modal tune, rather than marking its key signature as straightforward major or minor,"  ++
  " you can instead use the mode name. For example, the following tune is in D Mixolydian.  But remember, the classical" ++
  " modes all use the standard diatonic scale - they just start at different places along the scale. So for this tune " ++
  " the printed score would look, to all intents and purposes, identical to that for E Minor. Feel free to use either signature."

xmplMixolydian =
  "X: 1\r\nT: The Yellow Wattle\r\nR: jig\r\nM: 6/8\r\nL: 1/8\r\nK: Dmix\r\n" ++
  "|:dcA AGE|ABA ABc|dcA ABc|dcA AGE|\r\n" ++
  "dcA AGE|ABA AGE|EDD cde|dcA GED:|\r\n" ++
  "|:DED c3|ded c3|DED cde|dcA GED|\r\n" ++
  "DED c3|ded d2c|ABA ABc|dcA GED:|\r\n"

instKlezmer =
  "Klezmer tends to use modes that are not diatonic scales - some intervals are more than two semitones." ++
  " Suppose you have a tune that would be in a 'standard' mode except that one note in the scale is sharpened." ++
  " You can either use the name of the mode in the key signature and then explicitly sharpen this note each time it occurs in the score" ++
  " or you can modify the key signature itself, adding as many (sharpened or flattened) accidentals as are needed." ++ 
  " The following tune is in D Dorian, but with every G sharpened."

xmplKlezmer = 
  "X: 1\r\nT: Der Badchen Freylach \r\nM: 2/4\r\nL: 1/16\r\nK: Ddor^G\r\n" ++
  "|: DA,DF GAGF | A2A2 FED2 | DA,DF GAGF | A4 A4- |\r\n" ++
  "| AA,DF GAFD | A2A2 FED2 | EFGF EDEF | D8 :|\r\n" ++
  "|: ABcB dcBA | GABc A4 | dcBA GABc | A4 A4 |\r\n" ++
  "| ABcB dcBA | GABc A4 |1 ABcB AB (3FED | EFD2- D4 |\r\n" ++
  ":|2 GABA GAFE | D8 :||\r\n"

instBalkan =
 "Balkan music also tends to have unusual modes and time signatures.  This tune is in A Minor with a sharpened G; the meter is 11/16." ++
 " The '~' symbol indicates a particular decoration - a roll - but this player does not attempt it."

xmplBalkan =
  "X: 1\r\nT: Acano mlada nevesto\r\nO: Macedonia\r\nS: R.B.Iverson\r\nM: 11/16\r\nL: 1/16\r\nK: AMin^G\r\n" ++
  "|: E3  e2e2 d2c2 | ~B2A ~A2GA B2-B2 :: A2A dccB BAAG |\r\n" ++
  "| G2F ~F2EF ~G2FE | A2A dccB BAAG | ~G2F ~FGFE E2E2 :|\r\n" ++
  "|: EFD EFGA BcBA | Bcd cBcA BEeE |\r\n" ++
  "|  EFD EFGA BcBA | GAB AGFE E2-E2 :|\r\n"



lessons : Array Lesson 
lessons =
  [
    { title = "the notes", instruction = instNotes, example = xmplNotes, hint = hintNotes }
  , { title = "bars and rests", instruction = instBarsAndRests, example = xmplBarsAndRests, hint = hintBarsAndRests }
  , { title = "octaves", instruction = instOctaves, example = xmplOctaves, hint = "" }
  , { title = "long notes", instruction = instLongNotes, example = xmplLongNotes, hint = hintLongNotes }
  , { title = "short notes", instruction = instShortNotes, example = xmplShortNotes, hint = "" }
  , { title = "hornpipes", instruction = instHornpipe, example = xmplHornpipe, hint = hintHornpipe }
  , { title = "strathspeys", instruction = instStrathspey, example = xmplStrathspey, hint = "" }
  , { title = "chords", instruction = instChords, example = xmplChords, hint = "" }
  , { title = "key signature", instruction = instKeySig, example = xmplKeySig, hint = "" }
  , { title = "sharp and flat key signatures", instruction = instFlatKeySig, example = xmplFlatKeySig, hint = "" }
  , { title = "naturals", instruction = instNaturals, example = xmplNaturals, hint = "" }
  , { title = "sharps and flats", instruction = instAccidentals, example = xmplAccidentals, hint = "" }
  , { title = "how long is a unit note?", instruction = instUnitNote, example = xmplUnitNote, hint = "" }
  , { title = "tempo", instruction = instTempo, example = xmplTempo, hint = "" }
  , { title = "meter", instruction = instMeter, example = xmplMeter, hint = "" }
  , { title = "tie", instruction = instTie, example = xmplTie, hint = "" }
  , { title = "triplet", instruction = instTriplet, example = xmplTriplet, hint = "" }
  , { title = "triplet with differing note lengths", instruction = instComplexTriplet, example = xmplComplexTriplet, hint = "" }
  , { title = "quadruplet", instruction = instQuadruplet, example = xmplQuadruplet, hint = "" }
  , { title = "repeats", instruction = instRepeat, example = xmplRepeat, hint = "" }
  , { title = "repeats with variant endings", instruction = instRepeatVariants, example = xmplRepeatVariants, hint = "" }
  , { title = "tune title", instruction = instTitle, example = xmplTitle, hint = "" }
  , { title = "rhythm", instruction = instRhythm, example = xmplRhythm, hint = "" }
  , { title = "information headers", instruction = instInformation, example = xmplInformation, hint = "" }
  , { title = "key changes", instruction = instChangeKey, example = xmplChangeKey, hint = "" }
  , { title = "other modes", instruction = instMixolydian, example = xmplMixolydian, hint = "" }
  , { title = "klezmer", instruction = instKlezmer, example = xmplKlezmer, hint = "" }
  , { title = "Balkan", instruction = instBalkan, example = xmplBalkan, hint = "" }
  ] |> Array.fromList



