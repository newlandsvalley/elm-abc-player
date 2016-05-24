elm-abc-player
==============

This project explores the possibilities of playing [ABC notation](http://abcnotation.com/) directly in the browser within an Elm (0.17) application.  It uses two Elm libraries:

*   Elm-abc-parser.  This is a parser for ABC notation. 

*   SoundFont-ports.  This is the soundfont wrapper from [elm-soundfont-ports](https://github.com/newlandsvalley/elm-soundfont-ports).  It is more a psuedo-library because it relies on elm ports rather than a packaged library.

The idea is to take ABC input and play it directly in the browser (i.e. without there being any need for server-side rendering or generation of intermediate MIDI files).


Projects
--------

#### Simple Player

[Simpleplayer](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/simpleplayer) is a simple ABC file player (it plays a Swedish tune called 'Lillasystern').  It first loads the acoustic grand piano soundfont, loads and parses the ABC file and converts this into a performance simply by accumulating the elapsed times of each 'Note' event. It then converts each of these to a playable 'sound bite' attached to the appropriate soundfont and plays them as a single uninterruptable Task. 

to build:

    ./compilep.sh

to run, use:

    simpleplayer.html

#### ABC Editor

[Editor](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/editor) is a simple editor for ABC Scores.  It will allows you to edit ABC text, play the score or transpose it to another key. 

to build:

    ./compilee.sh

to run, use:

    abceditor.html

#### Interactive ABC Tutorial

In [tutorial](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/tutorial), the idea is to use the simple player but to take ABC input directly from the user.  She is taken through a succession of exercises, each of which gives a short tutorial on an aspect of ABC together with a tiny ABC sample which illustrates it.  She can play the sample and then tinker with it to see the effect that her changes make.  Samples start simply with just a few notes and end with a fully-fledged traditional tune illustrating key signatures, tempi, notes of different pitch and duration, triplets, chords, articulation and so on.

to build:

    ./compilet.sh

to run, use:

    abctutorial.html









 




