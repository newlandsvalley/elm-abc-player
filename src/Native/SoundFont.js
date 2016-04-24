Elm.Native.SoundFont = {};
Elm.Native.SoundFont.make = function (localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.SoundFont = localRuntime.Native.SoundFont || {};

    if (Elm.Native.SoundFont.values) return Elm.Native.SoundFont.values;	
   
    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);      
    var Maybe = Elm.Maybe.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);     

    var values = {};
    values.soundfontSignal = NS.constant(Maybe.Nothing);
   

    /* is the browser web-audio enabled? */
    values.isWebAudioEnabled = function() {
      try {
        console.log("Is web-audio enabled ?");
        var webAudio = new (window.AudioContext || window.webkitAudioContext);
        if (webAudio) {
          console.log("yes");
          return true;
        }
        else {
          console.log("no");
          return false;
        }
      }
      catch(err) {
          console.log("no after excception");
          return false;
      }
    }   

    /* can the browser play ogg format? */
    values.canPlayOgg = function() {
      var audioTester = document.createElement("audio");
      if (audioTester.canPlayType('audio/ogg')) {
        console.log("browser supports ogg");
        return true;
      }
      else {
        console.log("browser does not support ogg");
        return false;
      }
    }

    /* Get the audio context */
    values.getAudioContext = function() {
        console.log("get Audio Context");
        return new (window.AudioContext || window.webkitAudioContext)();
    };

    /* Get the current time from the audio context */
    values.getCurrentTime = function(context) {
      return context.currentTime;
    };

    /*
     * nameToUrl
     * Given an instrument name returns a URL to its Soundfont js file
     * (we only use acoustic grand piano at the moment)
     * load ogg format by preference if browser supports it
     *
     * @param {String} name - instrument name
     * @returns {String} the Soundfont data url
     */
     values.nameToUrl = function(name) {
       if (values.canPlayOgg()) {
         return 'assets/soundfonts/' + name + '-ogg.js';
       }
       else {
         return 'assets/soundfonts/' + name + '-mp3.js';
       }
     }

    /*
     * SoundFont.getScript
     *
     * Given a script URL returns a Promise with the script contents as text
     * @param {String} url - the URL
     */
    values.loadData = function(url) {
      return new Promise(function(done, reject) {
        var req = new XMLHttpRequest();
        req.open('GET', url);

        req.onload = function() {
          if (req.status == 200) {
            done(req.response);
          } else {
            reject(Error(req.statusText));
          }
        };
        req.onerror = function() {
          reject(Error("Network Error"));
        };
        req.send();
      });
    }

    /*
     *  Parse the SoundFont data and return a JSON object
     *  (SoundFont data are .js files wrapping json data)
     *
     * @param {String} data - the SoundFont js file content
     * @returns {JSON} the parsed data as JSON object
     */
    values.dataToJson = function(data) {
      var begin = data.indexOf("MIDI.Soundfont.");
      begin = data.indexOf('=', begin) + 2;
      var end = data.lastIndexOf(',');
      return JSON.parse(data.slice(begin, end) + "}");
    }

    /*
     * @param {Context} context - The Audio Context
     * @param {String} name - The bank name
     * @param {Object} data - The Soundfont instrument data as JSON
     */
    function createBank(context, name, data) {
      /* console.log("createBank in context: ", context); */
      var bank = { ctx: context, name: name, data: data };
      bank.buffers = {};

      return bank;
    }


   /*
    * INTENAL: decodeBank
    * Given a soundfont bank, returns a Promise that resolves when
    * all the notes from the instrument are decoded
    */
   function decodeBank(bank) {
      var promises = Object.keys(bank.data).map(function(note) {
        return decodeNote(bank.ctx, bank.data[note])
         .then(function(buffer) {          
           note = parseNote(note); 
           noteName = (note.midi).toString();
           /* console.log("decodeBank note: ", note.name); */
           localRuntime.notify(values.soundfontSignal.id, Maybe.Just(values.createAudioBuffer(noteName, buffer)));
        });
      });

      return Promise.all(promises).then(function() { 
        localRuntime.notify(values.soundfontSignal.id, Maybe.Just(values.createAudioBuffer("end", null)));
        /* console.log("returning after all decodeBank promises"); */
      })
    } 

    /*
     * Given a WAA context and a base64 encoded buffer data returns
     * a Promise that resolves when the buffer is decoded
     */
    function decodeNote(context, data) {
      return new Promise(function(done, reject) {
        var decodedData = base64DecodeToArray(data.split(",")[1]).buffer;
        context.decodeAudioData(decodedData, function(buffer) {
          done(buffer);
        }, function(e) {
          reject("DecodeAudioData error", e);
        });
      });
    }

    /* Create an AudioBuffer named after the note it provides */
    values.createAudioBuffer = function(name, buffer) {
      return {ctor: "AudioBuffer", "name" : name, "buffer" : buffer};
    };


    /*
     * loadSoundFonts
     *
     * Given an implicit Web Audio context and a instrument name
     * load the instrument data and generate a signal of sound sample audio buffers
     *
     * @param {Context} context - the Audio Context
     * @param {String} name - the soundfont instrument name
     */
 
    values.loadSoundFont = F2(function(context, name) {
      var promise = Promise.resolve(name)
        .then(values.nameToUrl)
        .then(values.loadData)
        .then(values.dataToJson)
        .then(function(jsonData) {
          return createBank(context, name, jsonData)
        })
        .then(decodeBank); 
       return values.soundfontSignal;
    });

 
    function b64ToUint6 (nChr) {
      return nChr > 64 && nChr < 91 ?
          nChr - 65
        : nChr > 96 && nChr < 123 ?
          nChr - 71
        : nChr > 47 && nChr < 58 ?
          nChr + 4
        : nChr === 43 ?
          62
        : nChr === 47 ?
          63
        :
          0;

    }

    // Decode Base64 to Uint8Array
    // ---------------------------
    function base64DecodeToArray(sBase64, nBlocksSize) {
      var sB64Enc = sBase64.replace(/[^A-Za-z0-9\+\/]/g, "");
      var nInLen = sB64Enc.length;
      var nOutLen = nBlocksSize ?
        Math.ceil((nInLen * 3 + 1 >> 2) / nBlocksSize) * nBlocksSize :
        nInLen * 3 + 1 >> 2;
      var taBytes = new Uint8Array(nOutLen);

      for (var nMod3, nMod4, nUint24 = 0, nOutIdx = 0, nInIdx = 0; nInIdx < nInLen; nInIdx++) {
        nMod4 = nInIdx & 3;
        nUint24 |= b64ToUint6(sB64Enc.charCodeAt(nInIdx)) << 18 - 6 * nMod4;
        if (nMod4 === 3 || nInLen - nInIdx === 1) {
          for (nMod3 = 0; nMod3 < 3 && nOutIdx < nOutLen; nMod3++, nOutIdx++) {
            taBytes[nOutIdx] = nUint24 >>> (16 >>> nMod3 & 24) & 255;
          }
          nUint24 = 0;
        }
      }
      return taBytes;
    }

/* PARSE NOTE */

    var NOTE = /^([a-gA-G])(#{0,2}|b{0,2})(-?[0-9]{1}|[+]{0,2}|[-]{0,2})$/
    /*
     * parseNote
     *
     * @param {String} note - the note string to be parsed
     * @return {Object} a object with the following attributes:
     * - pc: pitchClass, the letter of the note, ALWAYS in lower case
     * - acc: the accidentals (or '' if no accidentals)
     * - oct: the octave as integer. By default is 4
     */
    var parseNote = function (note, defaultOctave, defaultValue) {
      var parsed, match, octave

      // in scientific notation middleC is 4
      defaultOctave = defaultOctave || 4
      // test string against regex
      if (typeof note === 'string' && (match = NOTE.exec(note))) {
        // match[3] is the octave part
        if (match[3].length > 0 && !isNaN(match[3])) {
          octave = +match[3]
        } else if (match[3][0] === '+') {
          octave = defaultOctave + match[3].length
        } else if (match[3][0] === '-') {
          octave = defaultOctave - match[3].length
        } else {
          octave = defaultOctave
        }
        parsed = { pc: match[1].toLowerCase(),
          acc: match[2], oct: octave }
      } else if (typeof note.pc !== 'undefined'
        && typeof note.acc !== 'undefined'
        && typeof note.oct !== 'undefined') {
        parsed = note
      }

      if (parsed) {
        parsed.name = parsed.name || '' + parsed.pc + parsed.acc + parsed.oct
        parsed.midi = parsed.midi || toMidi(parsed)
        parsed.freq = parsed.freq || midiToFrequency(parsed.midi)
        return parsed
      } else if (typeof (defaultValue) !== 'undefined') {
        return defaultValue
      } else {
        throw Error('Invalid note format: ' + note)
      }
    }

    var SEMITONES = {c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 }

    function toMidi (note) {
      var alter = note.acc.length
      if (note.acc[0] === 'b') alter = -1 * alter
      return SEMITONES[note.pc] + alter + 12 * (note.oct + 1) 
    }

    function midiToFrequency (note) {
      return Math.pow(2, (note - 69) / 12) * 440
    }  

/* END OF PARSE NOTE */
 
    /* play an audio buffer at the supplied time offet and with appropriate volume (gain) */
    values.play = F4(function (context, buffer, time, gain) {
        /* console.log("buffer to play: " + buffer + " time: " + time + " with gain: " + gain) */
            return Task.asyncFunction(function (callback) {
                playSound(context, buffer, time, gain)
                callback(Task.succeed(Utils.Tuple0));
            });
        });

    function playSound(context, buffer, time, gain) { 
      // console.log("playing buffer at time: " + time + " with gain: " + gain)
      var source = context.createBufferSource(); 
      var gainNode = context.createGain();
      gainNode.gain.value = gain;
      source.buffer = buffer;
      source.connect(gainNode);
      gainNode.connect(context.destination)
      source.start(time);
    }


    return Elm.Native.SoundFont.values = values;       
};
