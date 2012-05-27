{-# LANGUAGE QuasiQuotes #-}
module Main where

import Language.Splinter

fileName = "test/Module.splinter"

testText = readFile(fileName)

main :: IO ()
main = do
  text <- testText
  print $ parseSplinter fileName text
