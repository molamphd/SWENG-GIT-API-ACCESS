module Lib
    ( someFunc
    ) where

import GitHub

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  testGitHubCall
  putStrLn "end."


testGitHubCall :: IO ()
testGitHubCall = do
  putStrLn "I would do a call here, so I would."
