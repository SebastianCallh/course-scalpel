module Hlint where

import           Turtle

run :: IO ()
run = shell "hlint src  --ignore 'Use $>'" ""
   *> shell "hlint test" ""
   *> shell "hlint cli"  ""
   *> pure ()
