module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)

import GifGameOfLife (gifGameOfLife)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (do putStrLn "need to provide a gif file as argument."; exitFailure)
  let inFile = head args
      outFile = replaceExtension inFile "out.gif"
  gifGameOfLife inFile outFile
