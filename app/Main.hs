module Main where
import Lib (discordMain)
import System.Environment ( getArgs )

main :: IO ()
main = do token <- readFile ".token"
          discordMain token
