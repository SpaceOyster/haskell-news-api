module Main where

import API (app)
import App.Env (Env (Env))
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  run 8081 (app Env)
