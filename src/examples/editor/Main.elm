import Effects exposing (Never)
import AbcEditor exposing (init, update, view, signals)
import StartApp
import Task exposing (Task)

app =
  StartApp.start
    { init = init "abc editor"
    , update = update
    , view = view
    , inputs = signals
    }

main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks


