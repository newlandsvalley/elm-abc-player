module Abc
    (  parse
    ,  parseError
    ,  ParseError
    ) where

{-|  Library for parsing ABC transcriptions using parser combinators
     see http://abcnotation.com/wiki/abc:standard:v2.1

# Definition

# Functions
@docs parse, parseError

# Types
@docs ParseError

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (..)
import Combine.Extra exposing (manyTill', leftBiasedOr)
import Ratio exposing (Rational, over, fromInt)
import String exposing (fromList, toList, foldl)
import Char exposing (fromCode, toCode, isUpper)
import Debug exposing (..)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (join)
import Dict exposing (Dict, get)
import Result exposing (Result)
import Regex exposing (Regex, contains)
import Abc.ParseTree exposing (..)


{-| a parse error with context -} 
type alias ParseError =
  {  msgs : List String
  ,  input : String
  ,  position : Int
  }

-- top level parsers
abc : Parser AbcTune
abc = (,) <$> headers <*> body 

-- BODY
body : Parser (List BodyPart)
body = (::) <$> 
    score <*> manyTill'
      (score `leftBiasedOr` tuneBodyHeader)
        end

score : Parser BodyPart 
score = Score <$> manyTill' scoreItem eol
          
scoreItem : Parser Music
scoreItem = rec <| \() -> 
    log "score item" <$>
    (
    choice 
       [ 
         chord               -- I think we need it placed before barline because of ambiguity of '['
       , inline              -- ditto
       , barline
       , brokenRhythmPair    -- must place before note because of potential ambiguity of AbcNote
       , note
       , rest
       , tuplet
       , slur
       , graceNote           -- we are not enforcing the ordering of grace notes, chords etc pre-note
       , annotation
       , chordSymbol
       , decoration
       , spacer
       , ignore
       , continuation
       ]       
    )
      <?> "score item"   


{- a bar line (plus optional repeat iteration marker)
   see comments in 4.8 Repeat/bar symbols:
   Abc parsers should be quite liberal in recognizing bar lines. In the wild, bar lines may have 
   any shape, using a sequence of | (thin bar line), [ or ] (thick bar line), and : (dots), e.g. |[| or [|::: 
-}
barline : Parser Music
barline = buildBarline <$> barSeparator <*> maybe Combine.Num.digit
             <?> "barline"

{- written like this instead of a regex because it's all regex control character! -}
barSeparator : Parser String
barSeparator = 
  String.fromList <$>
    (
    many1 <|
      choice 
        [ char '|'
        , char '['
        , char ']'
        , char ':'
        ]
    )       

slur : Parser Music
slur = Slur <$> choice [char '(', char ')']
             <?> "slur"      

{- Note, Slur should really be defined as Slur (List Music) and then parsed as shown below.  This would allow slurs to be
   nested and the parser to test that the brackets are balanced.  However, unfortunately, in the wild there are examples
   of slurs that go across music lines which make this interpretation impossible.  We thus simply parse the bracket characters.

   lazy evaluation of this unimplemented slur - comments courtesy of Bogdanp.
   Elm is eagerly evaluated so slur and slurContent end up creating a circular dependency that gets evaluated as soon as the output JS does, 
   which means one function ends up calling the constructor of the other before the other is defined. The rec combinator introduces
   laziness to get round the problem


slur : Parser Music
slur = rec <| \() -> 
        Slur <$>  parens (many1 musicItem)
             <?> "slur"
-}


brokenRhythmTie : Parser Broken
brokenRhythmTie  = buildBrokenOperator <$> regex "(<+|>+)"

brokenRhythmPair : Parser Music
brokenRhythmPair = BrokenRhythmPair <$> abcNote <*> brokenRhythmTie <*> abcNote
             <?> "broken rhythm pair"

rest : Parser Music
rest = Rest <$> (withDefault (fromInt 1) <$> (regex "[XxZz]" *> maybe noteDur))
             <?> "rest"
      

{- a free - format chord symbol - see 4.18 Chord symbols -}
chordSymbol : Parser Music
chordSymbol = ChordSymbol <$> quotedString
                 <?> "chord symbol"

{- an annotation to the score 
  4.19 Annotations

  General text annotations can be added above, below or on the staff in a similar way to chord symbols. In this case, the string within double quotes 
  is preceded by one of five symbols ^, _, <, > or @ which controls where the annotation is to be placed; above, below, to the left or right respectively
  of the following note, rest or bar line. Using the @ symbol leaves the exact placing of the string to the discretion of the interpreting program.
  These placement specifiers distinguish annotations from chord symbols, and should prevent programs from attempting to play or transpose them. 
  All text that follows the placement specifier is treated as a text string.

  Example: 

  "<(" ">)" C

-}
annotation : Parser Music
annotation = buildAnnotation <$> annotationString
                 <?> "annotation"

chord : Parser Music
chord = Chord <$> abcChord
                 <?> "chord"
        

inline : Parser Music
inline = Inline <$> 
           between (char '[') (char ']') tuneBodyInfo
                 <?> "inline header"

graceNote : Parser Music
graceNote =  between (char '{') (char '}') grace
               <?> "grace note"

grace : Parser Music
grace = GraceNote <$> acciaccatura <*> choice [noteSequence, chord]

{- acciaccaturas are indicated with an optional forward slash -}
acciaccatura : Parser Bool
-- acciaccatura = withDefault False <$> ( (\_ -> True) <$> maybe (char '/'))
acciaccatura = (\_ -> True) <$> maybe (char '/')

decoration : Parser Music
decoration = Decoration <$> choice [shortDecoration, longDecoration]
                  <?> "decoration"

shortDecoration : Parser String
shortDecoration = regex "[\\.~HLMOPSTuv]"
                  <?> "short decoration"

longDecoration : Parser String
longDecoration =  between (char '!') (char '!') (regex "[^\r\n!]*")
               <?> "long decoration"

{- just a sequence of Notes -}
noteSequence : Parser Music
noteSequence = NoteSequence <$> many1 note

-- general attributes
-- e.g 3/4
rational : Parser Rational
rational = Ratio.over <$> int <* char '/' <*> int

-- e.g. /4 (as found in note durations)
curtailedRational : Parser Rational
curtailedRational = Ratio.over 1 <$> (char '/' *> int)

{- e.g. / or // or /// (as found in note durations)
   which translates to 1/2, 1/4, 1/8 etc
-}
slashesRational : Parser Rational
slashesRational = 
   buildRationalFromExponential <$> (List.length <$> (many1 <| char '/'))


-- HEADER ATTRIBUTES

-- rational with trailing optional spaces
headerRational : Parser Rational
headerRational = rational <* whiteSpace

{- experimental -}
meterDefinition : Parser (Maybe MeterSignature)
meterDefinition =
  choice 
   [
     cutTime
   , commonTime
   , meterSignature
   , nometer
   ]

commonTime : Parser (Maybe MeterSignature)
commonTime = succeed (Just (4,4)) <* char 'C'

cutTime : Parser (Maybe MeterSignature)
cutTime = succeed (Just (2,2)) <* string "C|"

-- can't use Rationals for these because they cancel
meterSignature : Parser (Maybe MeterSignature)
meterSignature = Just <$> ( (,) <$> int <* char '/' <*> int <* whiteSpace)

nometer : Parser (Maybe MeterSignature)
nometer = succeed Nothing <* string "none"

noteDuration : Parser NoteDuration
noteDuration = rational <* whiteSpace

{- This is an area where we've relaxed the 2.1 spec
   1) we use many rather than many1 note length designators
   2) equals is now optional
   This means we can parse all of
        1/4 3/8 1/4 3/8=40
        "Allegro" 1/4=120
        3/8=50 "Slowly"
   but also the outdated (but prevalent in the wild
   120  (which means 1/4=120)
-}
tempoSignature : Parser TempoSignature
tempoSignature = buildTempoSignature <$> maybe spacedQuotedString <*> many headerRational <*> maybe (char '=') <*> int <*> maybe spacedQuotedString

-- accidental in a key signature (these use a different representation from accidentals in the tune body)
sharpOrFlat : Parser Accidental
sharpOrFlat = 
   map  (\x -> if x == '#' then Sharp else Flat)          
           (choice [ char '#', char 'b'])

keyName : Parser String
keyName =  regex "[A-G]"

keySignature : Parser KeySignature
keySignature = 
  buildKeySignature <$> keyName <*> maybe sharpOrFlat <*> maybe mode

-- an accidental as an amendment to a key signature - as in e.g. K:D Phr ^f
keyAccidental : Parser KeyAccidental
keyAccidental = buildKeyAccidental <$> accidental <*> pitch

-- of which there may be zero or more, separated by spaces
keyAccidentals : Parser (List KeyAccidental)
keyAccidentals = many (space *> keyAccidental)

mode : Parser Mode
mode = choice 
         [ major
         , ionian
         , dorian
         , phrygian
         , lydian
         , mixolydian
         , aeolian
         , locrian
         , minor   -- place last because of potential ambiguity	
         ]


minor : Parser Mode
-- minor = succeed Minor <* whiteSpace <* regex "(M|m)(I|i)(N|n)([A-Za-z])*"
minor = succeed Minor <* whiteSpace <* regex "(M|m)([A-Za-z])*"

major : Parser Mode
major = succeed Major <* whiteSpace <* regex "(M|m)(A|a)(J|j)([A-Za-z])*"

ionian : Parser Mode
ionian = succeed Ionian <* whiteSpace <* regex "(I|i)(O|o)(N|n)([A-Za-z])*"

dorian : Parser Mode
dorian = succeed Dorian <* whiteSpace <* regex "(D|d)(O|o)(R|r)([A-Za-z])*"

phrygian : Parser Mode
phrygian = succeed Phrygian <* whiteSpace <* regex "(P|p)(H|h)(R|r)([A-Za-z])*"

lydian : Parser Mode
lydian = succeed Lydian <* whiteSpace <* regex "(L|l)(Y|y)(D|d)([A-Za-z])*"

mixolydian : Parser Mode
mixolydian = succeed Mixolydian <* whiteSpace <* regex "(M|m)(I|i)(X|x)([A-Za-z])*"
 
aeolian : Parser Mode
aeolian = succeed Aeolian <* whiteSpace <* regex "(A|a)(E|e)(O|o)([A-Za-z])*"

locrian : Parser Mode
locrian = succeed Locrian <* whiteSpace <* regex "(L|l)(O|o)(C|c)([A-Za-z])*"
 
 
-- Headers
area : Parser Header
area = Area <$> ((headerCode 'A') *> strToEol)
               <?> "A header"

book : Parser Header
book = Book <$> ((headerCode 'B') *> strToEol)
               <?> "B Header"

composer : Parser Header
composer = Composer <$> ((headerCode 'C') *> strToEol)
               <?> "C header"

discography : Parser Header
discography = Discography <$> ((headerCode 'D') *> strToEol)
               <?> "D header"

fileUrl : Parser Header
fileUrl = FileUrl <$> ((headerCode 'F') *> strToEol)
               <?> "F header"

group : Parser Header
group = Group <$> ((headerCode 'G') *> strToEol)
               <?> "G header"

history : Parser Header
history = History <$> ((headerCode 'H') *> strToEol)
               <?> "H header"

instruction : Parser Header
instruction = Instruction <$> ((headerCode 'I') *> inlineInfo )
               <?> "I header"

key : Parser Header
key = buildKey <$> (headerCode 'K') <*> keySignature <*> keyAccidentals <* strToEol
               <?> "K header"

unitNoteLength : Parser Header
unitNoteLength = UnitNoteLength <$> ((headerCode 'L') *> noteDuration )
               <?> "L header"

meter : Parser Header
meter = Meter <$> ((headerCode 'M') *> meterDefinition  )
               <?> "M header"

macro : Parser Header
macro = Macro <$> ((headerCode 'm') *> inlineInfo)
               <?> "m header"

notes : Parser Header
notes = Notes <$> ((headerCode 'N') *> inlineInfo)
               <?> "N header"

origin : Parser Header
origin = Origin <$> ((headerCode 'O') *> strToEol)
               <?> "O header"

parts : Parser Header
parts = Parts <$> ((headerCode 'P') *> inlineInfo)
               <?> "P header"

tempo : Parser Header
tempo = Tempo <$> ((headerCode 'Q') *> tempoSignature)
               <?> "Q header"

rhythm : Parser Header
rhythm = Rhythm <$> ((headerCode 'R') *> inlineInfo)
               <?> "R header"

remark : Parser Header
remark = Remark <$> ((headerCode 'r') *> inlineInfo)
               <?> "r header"

source : Parser Header
source = Source <$> ((headerCode 'S') *> strToEol)
               <?> "S header"

symbolLine : Parser Header
symbolLine = SymbolLine <$> ((headerCode 's') *> inlineInfo)
               <?> "s header"

title : Parser Header
title = Title <$> ((headerCode 'T') *> inlineInfo)
               <?> "T header"

userDefined : Parser Header
userDefined = UserDefined <$> ((headerCode 'U') *> inlineInfo)
               <?> "U header"

voice : Parser Header
voice = Voice <$> ((headerCode 'V') *> inlineInfo)
               <?> "V header"

wordsAfter : Parser Header
wordsAfter  = WordsAfter  <$> ((headerCode 'W') *> inlineInfo)
               <?> "W header"

wordsAligned : Parser Header
wordsAligned  = WordsAligned  <$> ((headerCode 'w') *> inlineInfo)
               <?> "w header"

referenceNumber : Parser Header
referenceNumber = ReferenceNumber <$> ((headerCode 'X') *> int)
               <?> "x header"

transcription : Parser Header
transcription = Transcription <$> ((headerCode 'Z') *> strToEol)
               <?> "Z header"

{- a header is an information field up to and including the end of line marker -}
header : Parser Header
header = informationField <* eol

{- unsupported header reserved for future use -}
unsupportedHeader : Parser Header
unsupportedHeader = succeed UnsupportedHeader <* unsupportedHeaderCode <* strToEol
               <?> "unsupported header"

{- ditto for headers that may appear in the tune body -}
tuneBodyHeader : Parser BodyPart
tuneBodyHeader  = BodyInfo <$> tuneBodyInfo <* eol
                    <?> "tune body header"

{- whereas information fields can be used inline -}
informationField : Parser Header
informationField = 
  log "header" <$>
    (
    choice [ anywhereInfo
           , tuneInfo ]
             <?> "header"
    )
           
tuneInfo : Parser Header
tuneInfo = 
  choice [ area 
         , book
         , composer
         , discography
         , fileUrl
         , group
         , history
         , origin
         , source
         , referenceNumber
         , transcription 
         , unsupportedHeader  -- headers that are currently unsupported but must be recognized and ignored
         ]
           <?> "tune info"

anywhereInfo : Parser Header
anywhereInfo = 
  choice [ instruction 
         , key 
         , unitNoteLength 
         , meter
         , macro 
         , notes 
         , parts
         , tempo 
         , rhythm
         , remark 
         , title
         , userDefined
         , voice
         , wordsAfter
         , comment
         ]
            <?> "anywhere info"

tuneBodyOnlyInfo : Parser Header
tuneBodyOnlyInfo = 
  choice [ symbolLine 
         , wordsAligned ] 
           <?> "tune body only info"

tuneBodyInfo : Parser Header
tuneBodyInfo = 
  choice [ tuneBodyOnlyInfo
         , anywhereInfo ] 
           <?> "tune body info"

{- relax the spec in the pasring of headers to allow body-only tunes -}
headers : Parser TuneHeaders
headers = many header <?> "headers"
-- headers = many1 header <?> "headers"

{- comments.  These are introduced with '%' and can occur anywhere.
   The stylesheet directive '%%' is not recognized here and will
   simply be treated as a comment.  We'll treat comments as Headers
   so as not to pollute the parse tree overmuch
-}
comment : Parser Header
comment = Comment <$> (regex "%" *> strToEol)
               <?> "comment"

-- low level parsers
-- possible whitespace
whiteSpace : Parser String
whiteSpace = String.fromList <$> (many <| choice [space, tab])

-- at least one (intended) space somewhere inside the music body
spacer : Parser Music
spacer = Spacer <$> ( List.length <$> (many1 scoreSpace))
           <?> "space"

{- space within a line of the tune's score -}
scoreSpace : Parser Char
scoreSpace = choice [ space
            , char 'y'
            , tab
            ]

{- characters to ignore 

  Section 8.1 Tune Body:

  The following characters are currently reserved: # * ; ? @
  In future standards they may be used to extend the abc syntax. To ensure forward compatibility, 
  current software should ignore these characters when they appear inside or between note groups.

  section 4.7 Beams:

  Back quotes ` may be used freely between notes to be beamed, to increase legibility. 
  They are ignored by computer programs. For example, A2``B``C is equivalent to A2BC.
-}
ignore : Parser Music
ignore = succeed Ignore <* (regex "[#@;`\\*\\?]+")
               <?> "ignored character"

{- this is an area where the spec is uncertain.  See 6.1.1 Typesetting line-breaks
   The forward slash is used to indicate 'continuation of input lines' often because
   users may need to avoid long lines if, for example, they would otherwise extend
   beyond the limit of an old email system.  All very out of date, but nevertheless
   still prevalent in the wild.  We take the view that we must do our best to recognise 
   them and then throw them away (along with any other later stuff in the line)

   Return Continuation if we have a continuation
-}
continuation : Parser Music
continuation = succeed Continuation <* char '\\' <* regex "[^\r\n]*"
                  <?> "continuation"

headerCode : Char -> Parser Char
headerCode c = char c <* char ':' <* whiteSpace

unsupportedHeaderCode : Parser String
unsupportedHeaderCode = 
  regex "[a-qt-vx-zEJ]" <* char ':' <* whiteSpace

spacedQuotedString : Parser String
spacedQuotedString =
  whiteSpace *> quotedString <* whiteSpace

quotedString : Parser String
quotedString = 
    string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\"" 
      <?> "quoted string"

annotationString : Parser String
annotationString = 
    string "\"" *> regex "[\\^\\>\\<-@](\\\\\"|[^\"\n])*" <* string "\"" 
      <?> "annotation"

{- parse a remaining string up to but not including the end of line -}
strToEol : Parser String
-- strToEol = String.fromList <$> many (noneOf [ '\r', '\n' ]) 
strToEol = regex "[^\r\n]*"

{- parse an information item String - note that, because these can be used inline
   (bracketed by '[' and ']') it behoves us not to use the framing characters in the string
   not that the spec has anything to say about it as far as I can see
-}
inlineInfo : Parser String
inlineInfo = regex "[^\r\n\\[\\]]*"

note : Parser Music
note = Note <$> abcNote

abcNote : Parser AbcNote
abcNote = buildNote <$> maybeAccidental <*> pitch <*> moveOctave <*> maybe noteDur <*> maybeTie
               <?> "ABC note"

abcChord : Parser AbcChord
abcChord = buildChord <$> maybeAccidental <*>  (between (char '[') (char ']') (many1 abcNote)) <*> maybe noteDur 
               <?> "ABC chord"

{- an upper or lower case note ([A-Ga-g]) -}
pitch : Parser String
pitch = regex "[A-Ga-g]"

-- maybe an accidental defining a note's pitch
maybeAccidental : Parser (Maybe Accidental)
maybeAccidental = 
  maybe accidental

accidental : Parser Accidental
accidental = 
  buildAccidental <$>     
    (choice
      [ string "^^"
      , string "__"
      , string "^"
      , string "_"
      , string "="
      ]
     ) 

{- move an octave up (+ve - according to the number of apostrophes parsed)
             or down (-ve - according to the number of commas parsed)
-}
moveOctave : Parser Int
moveOctave =
   octaveShift <$> regex "[',]*"

{- count the number of apostrophe (up) or comma (down) characters in the string 
   and give the result a value of (up-down) 
-} 
octaveShift : String -> Int
octaveShift s =
  let 
    f c acc = case c of
      '\'' ->
        let 
          (up, down) = acc
        in
         (up+1, down)
      ',' ->
        let 
          (up, down) = acc
        in
         (up, down + 1)
      _ -> acc
    octs = String.foldl f (0,0) s
  in
    (fst octs - snd octs)

{- the duration of a note in the body -}
noteDur : Parser Rational
noteDur = 
   choice 
    [ rational                -- must come before integral as rational
    , integralAsRational
    , curtailedRational
    , slashesRational
    ]

{- now attached to leading note and not free-standing -}
maybeTie : Parser (Maybe Char)
maybeTie = (maybe (char '-'))
         <?> "tie"


integralAsRational : Parser Rational
integralAsRational =
   Ratio.fromInt <$> Combine.Num.digit

tuplet : Parser Music
tuplet = Tuplet <$> (char '(' *> tupletSignature) <*> many1 abcNote
               <?> "tuplet"

{- possible tuplet signatures
   (3             --> {3,2,3}
   (3:2           --> {3,2,3}
   (3::           --> {3,2,3}
   (3:2:4         --> {3,2,4}
   (3::2          --> {3,2,2}
-}
tupletSignature : Parser TupletSignature
tupletSignature = buildTupletSignature <$> 
   regex "[2-9]" <*> tup <*> tup

tup : Parser (Maybe String)
tup = join <$> maybe 
        (char ':' *> maybe (regex "[2-9]"))

-- builders

-- build a rationalal quantity - "x/y" -> Rational x y
{-
buildRational : Int -> Char -> Int -> MeterSignature
buildRational x slash y = x `over` y
-}

{- used in counting slashes exponentially -}
buildRationalFromExponential : Int -> Rational
buildRationalFromExponential i =
  Ratio.over 1 (2 ^ i)

-- build a tempo signature
buildTempoSignature : Maybe String -> List Rational -> Maybe Char -> Int -> Maybe String -> TempoSignature
buildTempoSignature ms1 fs c i ms2 =
   let ms = 
      case ms1 of
        Nothing -> ms2
        _ -> ms1
   in
    { noteLengths = fs
    , bpm = i
    , marking = ms
    }

{- Dictionary for PitchClass -}
pitchClassDict : Dict String PitchClass
pitchClassDict =
  Dict.fromList
    [ ("A", A),
      ("B", B),
      ("C", C), 
      ("D", D),
      ("E", E),
      ("F", F),
      ("G", G)
     ]

lookupPitch : String -> PitchClass
lookupPitch p =
  Dict.get (String.toUpper p) pitchClassDict
    |> withDefault C 

-- build a key signature
buildKeySignature : String -> Maybe Accidental -> Maybe Mode -> KeySignature
buildKeySignature pStr ma mm =
  { pitchClass = lookupPitch pStr, accidental = ma, mode = withDefault Major mm }

-- build a complete key designation (key signature plus modifyinhg accidentals)
buildKey : Char -> KeySignature -> List KeyAccidental -> Header
buildKey c ks ka = Key ks ka


{- build a bar line 
  this is a bit tricky because of the poor specification for the possible shapes of bar lines 
  which may have multiple different types of bar line markers (|,[,]) and repeat markers (:)  
  Try to normalise to representations of basic shapes like (|, |:, :|, :||, ||:, ||, :|:, :||: )
-}
buildBarline : String -> Maybe Int -> Music
buildBarline s i = 
  let 
    f c = case c of
      '[' -> '|'
      ']' -> '|'
      _ -> c
    -- normalise all lines to '|'
    normalised = String.map f s
    -- count the lines up to a maximum of 2, minimum of 1
    lines = String.length (String.filter (\c -> c == '|') normalised)
    normalisedLineCount = max (min lines 2) 1 
    -- count the repeat markers
    repeatCount = String.length (String.filter (\c -> c == ':') normalised)
    -- set the repeat
    repeat = 
       if (repeatCount == 0) then
         Nothing
       else if (repeatCount == 1) then
         if String.contains ":|" normalised then
           Just End
         else
           Just Begin
       else 
         Just BeginAndEnd
  in
    Barline { lines = normalisedLineCount, repeat = repeat, iteration = i }

  
buildNote : Maybe Accidental -> String -> Int -> Maybe Rational -> Maybe Char -> AbcNote
buildNote macc pitchStr octave ml mt = 
   let 
     l = withDefault (Ratio.fromInt 1) ml
     -- a = buildAccidental macc
     p = lookupPitch (String.toUpper pitchStr)
     spn = scientificPitchNotation pitchStr octave
     tied = case mt of
        Just _ -> True
        _ -> False
   in 
     { pitchClass = p, accidental = macc, octave = spn, duration = l, tied = tied }

{- investigate a note/octave pair and return the octave
   in scientific pitch notation (middle C = 4)
-}
scientificPitchNotation : String -> Int -> Int
scientificPitchNotation pc oct =
  if (contains (Regex.regex "[A-G]")) pc then  -- pitch class inhabits octave of middle C, oct <= 0
    4 + oct
  else                                         -- pitch class inhabits octave above middle C, oct >= 0
    5 + oct

buildAccidental : String -> Accidental
buildAccidental s = case s of
   "^^" -> DoubleSharp
   "__" -> DoubleFlat
   "^"  -> Sharp
   "_"  -> Flat
   _  -> Natural

buildKeyAccidental : Accidental -> String -> KeyAccidental
buildKeyAccidental a pitchStr =
  let
    pc = lookupPitch pitchStr
  in
    { accidental = a, pitchClass = pc }

buildChord : Maybe Accidental -> List AbcNote -> Maybe Rational ->  AbcChord
buildChord macc ns ml = 
  let
    l = withDefault (Ratio.fromInt 1) ml
    -- a = buildAccidental macc
  in 
    { notes = ns, accidental = macc, duration = l }

{- build a tuplet signature {p,q,r) - p notes in the time taken for q
   in operation over the next r notes
-}
buildTupletSignature : String -> Maybe String -> Maybe String -> TupletSignature
buildTupletSignature ps mq mr = 
  let 
    p = toTupletInt ps

    -- default values for q.  Not quite in accordance with spec where q varies
    -- between 2 and 3 for odd values of p, dependent on the time signature
    -- (but this would make the parser stateful which we don't want for such small
    -- edge cases)
    qdefault = case p of
      2 -> 3
      3 -> 2
      4 -> 3
      6 -> 2
      8 -> 3
      _ -> 2   
    q = withDefault qdefault (Maybe.map toTupletInt mq)
    r = withDefault p (Maybe.map toTupletInt mr)
  in
    (p,q,r)

buildBrokenOperator : String -> Broken
buildBrokenOperator s =
  if String.startsWith "<" s then
    LeftArrow (String.length s)
  else
    RightArrow (String.length s)

buildAnnotation : String -> Music
buildAnnotation s =
  let     
    firstChar = List.head ( String.toList s )
    placement = case firstChar of
     Just '^' -> AboveNextSymbol
     Just '_' -> BelowNextSymbol
     Just '<' -> LeftOfNextSymbol
     Just '>' -> RightOfNextSymbol
     _ -> Discretional
  in
    Annotation placement s


toTupletInt : String -> Int
toTupletInt s =
  s |> String.toInt
    |> Result.toMaybe
    |> withDefault 3   -- default can't happen because all strings are regex-parsed 2-9    

                
{- just for debug purposes - consume the rest of the input -}
{-
restOfInput : Parser (List Char)
restOfInput = many anyChar
-}
          
-- exported functions

{-| entry point - Parse an ABC tune image -}
parse : String -> Result.Result ParseError AbcTune
parse s =
  case Combine.parse abc s of
    (Ok n, _) ->
      Ok n

    (Err msgs, ctx) ->
      Err { msgs = msgs, input = ctx.input, position = ctx.position }

{-| format a parse error as a string -}
parseError : ParseError -> String
parseError pe =
  let
    append a b = a ++ "," ++ b
    msg = List.foldr append "" pe.msgs
  in
    "parse error: " ++ msg ++ " on " ++ pe.input ++ " at position " ++ toString (pe.position)




        
