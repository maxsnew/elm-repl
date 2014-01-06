{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Environment where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Monoid     ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char             as Char
import qualified Data.List             as List
import Data.Trie       (Trie)
import qualified Data.Trie             as Trie

data Repl = Repl {
    _compilerPath :: FilePath
  , _flags   :: [String]
  , _imports :: Trie String
  , _adts    :: Trie String
  , _defs    :: Trie String
  } deriving Show

makeLenses ''Repl

empty :: FilePath -> Repl
empty compilerPath =
  Repl compilerPath [] Trie.empty Trie.empty (Trie.singleton firstVar (BS.unpack firstVar <> " = ()"))

firstVar :: ByteString
firstVar = "tsol"

lastVar :: ByteString
lastVar = "deltron3030"

toElm :: Repl -> String
toElm env = unlines $ "module Repl where" : decls
    where decls = concatMap Trie.elems . map (env^.) $ [ imports, adts, defs ]

insert :: String -> Repl -> Repl
insert str
    | List.isPrefixOf "import " str = 
      let name = BS.pack . getFirstCap . words $ str
          getFirstCap (token@(c:_):rest) = if Char.isUpper c
                                           then token
                                           else getFirstCap rest
          getFirstCap _ = str
      in  noDisplay . (imports %~ Trie.insert name str)

    | List.isPrefixOf "data " str =
        let name = BS.pack . takeWhile (/=' ') . drop 5 $ str
        in  noDisplay . (adts %~ Trie.insert name str)
            
    | otherwise =
        case break (=='=') str of
          (_,"") -> display str
          (beforeEquals, _:c:_)
              | Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals -> display str
              | otherwise -> let name = declName $ beforeEquals
                             in  define (BS.pack name) str . display name
          _ -> error "Environment.hs: Case error. Submit bug report."
        where
          declName pattern =
              case takeWhile Char.isSymbol . dropWhile (not . Char.isSymbol) $ pattern of
                "" -> takeWhile (/=' ') pattern
                op -> op

          hasLet = elem "let" . map token . words
            where
              isVarChar c = Char.isAlpha c || Char.isDigit c || elem c "_'"
              token = takeWhile isVarChar . dropWhile (not . Char.isAlpha)

          hasBrace = elem '{'

define :: ByteString -> String -> Repl -> Repl
define name body = defs %~ Trie.insert name body

display :: String -> Repl -> Repl
display = define lastVar . format
  where format body = (BS.unpack lastVar) ++ " =" ++ concatMap ("\n  "++) (lines body)

noDisplay :: Repl -> Repl
noDisplay = defs %~ Trie.delete lastVar
