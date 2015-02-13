import Control.Monad.IO.Class
import Clac
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade


main :: IO ()
main = makeGUI


-- |The glade file to load the UI from.
gladeFile :: FilePath
gladeFile = "src-exec/clac.glade"


-- |Main entry point for the GTK GUI routines.
makeGUI :: IO ()
makeGUI = do
  -- init gui
  _ <- initGUI

  -- load glade file
  Just xml   <- xmlNew gladeFile

  -- get widgets
  mainWin     <- xmlGetWidget xml castToWindow "mainWin"
  inputEntry  <- xmlGetWidget xml castToEntry "inputEntry"
  outputEntry <- xmlGetWidget xml castToEntry "outputEntry"
  button1     <- xmlGetWidget xml castToButton "button1"
  button2     <- xmlGetWidget xml castToButton "button2"
  button3     <- xmlGetWidget xml castToButton "button3"
  button4     <- xmlGetWidget xml castToButton "button4"
  button5     <- xmlGetWidget xml castToButton "button5"
  button6     <- xmlGetWidget xml castToButton "button6"
  button7     <- xmlGetWidget xml castToButton "button7"
  button8     <- xmlGetWidget xml castToButton "button8"
  button9     <- xmlGetWidget xml castToButton "button9"
  button0     <- xmlGetWidget xml castToButton "button0"
  buttonPlus  <- xmlGetWidget xml castToButton "buttonPlus"
  buttonPlusX <- xmlGetWidget xml castToButton "buttonPlusX"
  buttonMinus <- xmlGetWidget xml castToButton "buttonMinus"
  buttonMul   <- xmlGetWidget xml castToButton "buttonMul"
  buttonDiv   <- xmlGetWidget xml castToButton "buttonDiv"
  buttonEq    <- xmlGetWidget xml castToButton "buttonEq"
  buttonAC    <- xmlGetWidget xml castToButton "buttonAC"
  buttonNeg   <- xmlGetWidget xml castToButton "buttonNeg"
  buttonSin   <- xmlGetWidget xml castToButton "buttonSin"
  buttonAsin  <- xmlGetWidget xml castToButton "buttonAsin"
  buttonCos   <- xmlGetWidget xml castToButton "buttonCos"
  buttonAcos  <- xmlGetWidget xml castToButton "buttonAcos"
  buttonTan   <- xmlGetWidget xml castToButton "buttonTan"
  buttonAtan  <- xmlGetWidget xml castToButton "buttonAtan"
  buttonPi    <- xmlGetWidget xml castToButton "buttonPi"

  _ <- onClicked buttonAC    (clearEntry inputEntry >>=
                               (\_ -> clearEntry outputEntry))
  _ <- onClicked buttonEq    $ calcGUI inputEntry outputEntry
  _ <- onClicked button1     $ updateInputEntry inputEntry "1"
  _ <- onClicked button2     $ updateInputEntry inputEntry "2"
  _ <- onClicked button3     $ updateInputEntry inputEntry "3"
  _ <- onClicked button4     $ updateInputEntry inputEntry "4"
  _ <- onClicked button5     $ updateInputEntry inputEntry "5"
  _ <- onClicked button6     $ updateInputEntry inputEntry "6"
  _ <- onClicked button7     $ updateInputEntry inputEntry "7"
  _ <- onClicked button8     $ updateInputEntry inputEntry "8"
  _ <- onClicked button9     $ updateInputEntry inputEntry "9"
  _ <- onClicked button0     $ updateInputEntry inputEntry "0"
  _ <- onClicked buttonPlus  $ updateInputEntry inputEntry "+"
  _ <- onClicked buttonPlusX $ updateInputEntry inputEntry "+"
  _ <- onClicked buttonMinus $ updateInputEntry inputEntry "-"
  _ <- onClicked buttonMul   $ updateInputEntry inputEntry "*"
  _ <- onClicked buttonDiv   $ updateInputEntry inputEntry "/"
  _ <- onClicked buttonNeg   $ updateInputEntry inputEntry "neg"
  _ <- onClicked buttonSin   $ updateInputEntry inputEntry "sin"
  _ <- onClicked buttonAsin  $ updateInputEntry inputEntry "asin"
  _ <- onClicked buttonCos   $ updateInputEntry inputEntry "cos"
  _ <- onClicked buttonAcos  $ updateInputEntry inputEntry "acos"
  _ <- onClicked buttonTan   $ updateInputEntry inputEntry "tan"
  _ <- onClicked buttonAtan  $ updateInputEntry inputEntry "atan"
  _ <- onClicked buttonPi    $ updateInputEntry inputEntry "pi"
  _ <- on inputEntry entryActivated $ calcGUI inputEntry outputEntry

  -- hotkeys
  _ <- mainWin `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- eventKeyName
    liftIO mainQuit

  -- draw widgets and start main loop
  widgetShowAll mainWin
  mainGUI


-- |Calculate given on the current input field and set the output field.
calcGUI :: Entry -> Entry -> IO ()
calcGUI input output = do
  text <- entryGetText input
  case solveEquation text of
    Just x  -> entrySetText output (show x)
    Nothing -> entrySetText output (if null text then "" else "Err")


-- |Update the input entry with the given string. Used if one of the
-- buttons (such as '3' or '+', etc.) are clicked.
updateInputEntry :: Entry -> String -> IO ()
updateInputEntry input str = do
  text <- entryGetText input
  entrySetText input (if null text then str else text ++ " " ++ str)


-- |Clear a given entry widget completely.
clearEntry :: Entry -> IO ()
clearEntry entry = entrySetText entry ""
