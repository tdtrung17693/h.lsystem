module Main (main) where

import Graphics.Gloss ( white, display, Display(InWindow) )
import Data.List (intersperse)
import Parser (Atom (..), AtomFnCall (AtomFnCall), Expr (BinOp, Number))
import Options.Applicative (Parser, switch, auto, option, long, short, help, info, (<**>), helper, fullDesc, header, execParser, showDefault, value, strOption, metavar, strArgument)
import Control.Monad (when)
import Executor
    ( exec, RunOptions(RunOptions, debugMode, unitAngle), RunState(debug) )

getText :: AtomFnCall -> String
getText (AtomFnCall (Atom a) args) = case args of
    [] -> a
    _ -> mconcat [a,"(", mconcat (intersperse "," (getExprText <$> args)),")"]

getExprText :: Expr -> String
getExprText (BinOp op op1 op2) = mconcat [getExprText op1, op, getExprText op2]
getExprText (Number n) = show n
getExprText _ = ""

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
        Right (st, ops, atoms, p) ->
            (when (debugMode options) (do
                (print (mconcat (map (\s->(getText s))atoms)))
                print (unitAngle ops)
                (putStrLn (mconcat . intersperse "\n" $ (debug st))))
            >> display (InWindow "L-System" (500, 500) (0,0)) white p)
        Left e -> print e) =<< execParser opts
    where opts = info (flags <**> helper)
            (fullDesc
            <> header "lsystem - A simple L-System executor" )
