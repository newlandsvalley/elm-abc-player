elm-abc-player
==============

These projects explore the possibilities of playing [ABC notation](http://abcnotation.com/) directly in the browser within an Elm (0.18) application.  They use the following Elm audio libraries:

*   Elm-abc-parser.  This is a parser for ABC notation.

*   Soundfont-ports.  This is the soundfont wrapper from [elm-soundfont-ports](https://github.com/newlandsvalley/elm-soundfont-ports).  It is more a psuedo-library because it relies on elm ports rather than a packaged library.

*   Midi-player.  This is a player for MIDI recordings packaged as an autonomous module.  It, in turn, relies on soundfont-ports and is used in projects where the ABC is first converted to MIDI.

*   Elm-vextab.  This is a wrapper around the [VexTab](https://github.com/0xfe/vextab) API into [VexFlow](https://github.com/0xfe/vexflow) which is a JavaScript library for rendering music notation.

The idea is to take ABC input and play it directly in the browser (i.e. without there being any need for server-side rendering or generation of intermediate MIDI files).

Projects
--------

In each of these projects, playback is attempted as simply as possible in order to give a clear, unornamented rendition of the tune.  The various notes and chords are played, but chord symbols, grace notes and other ornamentation forms are all ignored.

#### Simple Player

[Simpleplayer](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/simpleplayer) is a simple ABC file player (it plays a Swedish tune called 'Lillasystern').  It first loads the acoustic grand piano soundfont, loads and parses the ABC file and converts this into a performance by accumulating the elapsed times of each 'Note' event. It then converts each of these to a playable 'sound bite' attached to the appropriate soundfont and plays them as a single uninterruptable Task.

to build:

    ./compilep.sh

to run, use:

    simpleplayer.html

#### ABC Editor

[Editor](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/editor) is an editor for ABC Scores.  It allows you to edit ABC text, play the score or transpose it to another key.

to build:

    ./compilee.sh

to run, use:

    abceditor.html

#### Interactive ABC Tutorial

In [tutorial](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/tutorial), the idea is to use the simple player but to take ABC input directly from the user.  She is taken through a succession of exercises, each of which gives a short tutorial on an aspect of ABC together with a tiny ABC sample which illustrates it.  She can play the sample and then tinker with it to see the effect that her changes make.  Samples start with just a few notes and end with a fully-fledged traditional tune illustrating key signatures, tempi, notes of different pitch and duration, triplets, chords, articulation and so on. You can try the tutorial [here](http://www.tradtunedb.org.uk/abctutorial).

to build:

    ./compilet.sh

to run, use:

    abctutorial.html

#### ABC Editor with embedded MIDI player

[Editor-controller](https://github.com/newlandsvalley/elm-abc-player/tree/master/src/examples/editor-controller) is another version of the editor above.  However, this one translates the ABC into a [MidiRecording](https://github.com/newlandsvalley/elm-comidi/blob/master/src/MidiTypes.elm) and uses the [midi-player](https://github.com/newlandsvalley/midi-player) module to play the recording (which you can stop and start). It includes buttons that allow you to load and save the ABC file. It now also includes an experimental feature which attempts to display the growing score as the ABC is being edited using a renderer based on an alpha release of [VexTab](http://www.vexflow.com/vextab/). You can try the editor [here](http://www.tradtunedb.org.uk/abceditor).

to build:

    ./compileec.sh

to run, use:

    abceditorcontroller.html

Sources of native javascript code
---------------------------------

These projects (particularly the editor controller) integrate a number of modules together which use native javascript by means of ports.  This javascript is maintained in the js directory.  Sources are as follows:


| Name | Description | Github Repo | Maintainer email |
|------|-------------|-------------|------------------|
|  soundfont-player | play soundfonts using web-audio | https://github.com/danigb/soundfont-player | danigb@gmail.com |   
|  vextab-div | display scores | https://github.com/0xfe/vextab | http://0xfe.muthanna.com |   
|  nativeFileIO | Port for text file IO | https://github.com/newlandsvalley/elm-file-io | john.watson@gmx.co.uk |   
|  nativeSoundFont | Port wrapper round soundfont-player | https://github.com/newlandsvalley/elm-soundfont-ports | john.watson@gmx.co.uk |   
|  nativeVexTab | Port wrapper round vextab-div | https://github.com/newlandsvalley/elm-vextab | john.watson@gmx.co.uk |   

