module Main (main) where

import Graphics.Gloss ( white, display, Display(InWindow) )
import Data.List (intersperse)
import Parser (Atom (Id, Symb))
import Options.Applicative (Parser, switch, auto, option, long, short, help, info, (<**>), helper, fullDesc, header, execParser, showDefault, value, strOption, argument, metavar, Alternative (some), str, ReadM, optional, strArgument)
import Control.Monad (when)
import Executor
    ( exec, RunOptions(RunOptions, debugMode), RunState(debug) )
import System.Environment
import Data.Char (isSpace)

getText :: Atom -> String
getText (Id a) = a
getText (Symb a) = a

data CLIOptions = CLIOptions {
    runOptions :: RunOptions,
    file :: String
}

flags :: Parser CLIOptions
flags = CLIOptions <$> (RunOptions -- Float Float Integer String String
            <$> option auto
                ( long "unit-length"
                <> short 'u'
                <> showDefault
                <> value 32
                <> help "Unit length" )
            <*> pure 0.0
            <*> option auto
                ( long "number-of-generation"
                <> short 'n'
                <> showDefault
                <> value 2
                <> help "Number of generation to run")
            <*> strOption
                ( long "forward"
                <> short 'f'
                <> showDefault
                <> value ""
                <> help "Drawing forward symbols")
            <*> strOption
                ( long "backward"
                <> short 'b'
                <> showDefault
                <> value ""
                <> help "Drawing backward symbols")
            <*> switch
                (long "debug"
                <> short 'd'
                <> help "Debug mode - print all performed steps"))
        <*> strArgument (showDefault <> value "" <> (metavar "FILE"))

main :: IO ()
main = (\cliOptions -> do
    source <- if (file cliOptions) /= "" then readFile (file cliOptions) else getContents
    let options = runOptions cliOptions

    case exec source options of
        Right (st, atoms, p) ->
            (when (debugMode options) (
                print (foldr (\a s->s ++ (getText a)) "" atoms)
                >> (putStrLn (mconcat . intersperse "\n" $ (debug st)))))
            >> display (InWindow "L-System" (500, 500) (0,0)) white p
        Left e -> print e) =<< execParser opts
    where opts = info (flags <**> helper)
            (fullDesc
            <> header "lsystem - A simple L-System executor" )
