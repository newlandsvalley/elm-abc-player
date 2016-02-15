import Effects exposing (Never)
import AbcTutorial exposing (init, update, view, signals)
import StartApp
import Task exposing (Task)

app =
  StartApp.start
    { init = init "abc tutorial"
    , update = update
    , view = view
    , inputs = signals
    }

main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks


