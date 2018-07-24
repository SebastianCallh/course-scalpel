module Hlint where

import           Turtle

run :: IO ()
run =
  shell "hlint . --ignore 'Use $>'" ""
  *> pure ()
