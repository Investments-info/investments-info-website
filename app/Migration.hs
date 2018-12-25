module Migration where

import           Model
import           Universum

main :: IO ()
main = runDBA runMigrations
