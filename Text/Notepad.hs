import Data.IORef
import Data.Maybe
import Data.Char
import System.Exit
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Graphics.Vty.Widgets.All

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (a:as) (b:bs) = if a == b then startsWith as bs else False

argument :: String -> Maybe String
argument cmd
    | (length . words $ cmd) > 1 = Just ((words cmd) !! 1)
    | otherwise = Nothing

determineSavePath :: Maybe String -> Maybe String -> Maybe FilePath
determineSavePath (Just a) Nothing = Just a
determineSavePath Nothing (Just b) = Just b
determineSavePath (Just a) (Just b) = Just a -- favor command line path
determineSavePath Nothing Nothing = Nothing

dispatchCommand :: String -> Widget Edit -> Widget Edit -> IORef (Maybe String) -> IO ()
dispatchCommand cmd commandWidget editorWidget editorState
    | startsWith "open" cmd = do
        txt <- case argument cmd of
                    Just file -> do
                        setEditText commandWidget (T.pack "")
                        TIO.readFile $ file
                    Nothing -> do
                        setEditText commandWidget (T.pack "open <need a filename>")
                        return (T.pack "")
        modifyIORef editorState (\_ -> (argument cmd))
        setEditText editorWidget txt
    | startsWith "save" cmd = do
        curFileName <- readIORef editorState
        txt <- getEditText editorWidget
        case determineSavePath (argument cmd) curFileName of
            Just path -> do
                TIO.writeFile path txt
                setEditText commandWidget (T.pack "")
            Nothing -> do 
                setEditText commandWidget (T.pack "save <need a filename>")
                return ()
    | startsWith "quit" cmd = shutdownUi
    | otherwise = error "Unknown command"

main :: IO ()
main = do
    editor <- multiLineEditWidget
    command <- editWidget
    ui <- vBox editor command
    fg <- newFocusGroup
    addToFocusGroup fg editor
    addToFocusGroup fg command
    c <- newCollection
    _ <- addToCollection c ui fg
    curFile <- newIORef Nothing
    command `onActivate` \this -> do
        text <- getEditText this
        dispatchCommand (T.unpack text) command editor curFile
    runUi c defaultContext
