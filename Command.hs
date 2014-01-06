module Command (runCommand)
       where

import Control.Lens
import Data.Functor             ((<$>), (<$))
import Control.Monad.Trans      (liftIO)
import System.Exit              (ExitCode, exitSuccess)
import Text.Parsec hiding (getInput)

import qualified Data.List   as List

import qualified Environment as Env
import Monad

data Command
    = AddFlag String
    | RemoveFlag String
    | ListFlags
    | ClearFlags
    | InfoFlags
    | Help
    | Exit
    | Reset
    deriving Show

runCommand :: String -> ReplM (Maybe ExitCode)
runCommand raw =
  case parse commands "" raw of
    Right command -> handleCommand command
    Left err ->
      Nothing <$ (liftIO . putStrLn $
                  "Could not parse command '" ++ raw ++ "':\n" ++ show err)


handleCommand :: Command -> ReplM (Maybe ExitCode)
handleCommand command =
    case command of
      Exit      -> Just <$> liftIO exitSuccess
      Help      -> display helpInfo
      InfoFlags -> display flagsInfo
      ListFlags -> display . unlines =<< use Env.flags

      AddFlag flag ->
        modifyIfPresent True  flag "Added "        "Flag already added!" (Env.flags %= flip snoc flag)
      RemoveFlag flag ->
        modifyIfPresent False flag "Removed flag " "No such flag."       (Env.flags %= List.delete flag)

      Reset      -> modifyAlways "Environment Reset" (id %= Env.empty . (view Env.compilerPath))
      ClearFlags -> modifyAlways "All flags cleared" (Env.flags .= [])

  where display msg = Nothing <$ (liftIO . putStrLn $ msg)
        modifyIfPresent b flag msgSuc msgFail mod = do
          isElem <- elem flag <$> use Env.flags
          if (not b) `xor` isElem
            then display msgFail
            else Nothing <$ do
          liftIO . putStrLn $ msgSuc ++ flag
          mod
        modifyAlways msg mod = Nothing <$ do
          liftIO . putStrLn $ msg
          mod

xor :: Bool -> Bool -> Bool
xor True  = not
xor False = id

commands :: Parsec String () Command
commands = choice (flags : basics)
    where
      basics = map basicCommand [ ("exit",Exit), ("reset",Reset), ("help",Help) ]

      basicCommand (name,command) =
          string name >> spaces >> eof >> return command

flags :: Parsec String () Command
flags = do
  string "flags"
  many space
  choice [ srcDirFlag "add"    AddFlag
         , srcDirFlag "remove" RemoveFlag
         , ListFlags  <$ string "list"
         , ClearFlags <$ string "clear"
         , return InfoFlags
         ]
    where srcDirFlag name ctor = do
            string name
            many1 space
            ctor <$> srcDir

srcDir :: Parsec String () String
srcDir = do
  string "--src-dir="
  dir <- manyTill anyChar (choice [ space >> return (), eof ])
  return $ "--src-dir=" ++ dir

flagsInfo :: String
flagsInfo = "Usage: flags [operation]\n\
            \\n\
            \  operations:\n\
            \    add --src-dir=FILEPATH\tAdd a compiler flag\n\
            \    remove --src-dir=FILEPATH\tRemove a compiler flag\n\
            \    list\t\t\tList all flags that have been added\n\
            \    clear\t\t\tClears all flags\n" 

helpInfo :: String
helpInfo = "General usage directions: <https://github.com/evancz/elm-repl#elm-repl>\n\
           \Additional commands available from the prompt:\n\
           \\n\
           \  :help\t\t\tList available commands\n\
           \  :flags\t\tManipulate flags sent to elm compiler\n\
           \  :reset\t\tClears all previous imports\n\
           \  :exit\t\t\tExits elm-repl\n"
