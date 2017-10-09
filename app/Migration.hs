module Main where

import Prelude

import Model

main :: IO ()
main = do
  runDBA runMigrations
