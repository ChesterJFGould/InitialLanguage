module Main where

import qualified Sexpr as S
import qualified Syntax as Stx
import qualified SyntaxDesc as SD
import System.IO

main :: IO ()
main = do
  s <- hGetContents' stdin
  case SD.parse S.sexprs s >>= Stx.sexprs2Program of
    Just p -> print p
    Nothing -> print "Parse error"
