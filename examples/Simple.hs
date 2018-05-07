-- | Example usage of "Control.Eff.Operational" with TH - generated dsl operations
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Eff.Operational
import Control.Eff.Operational.TH (mkProgramOps)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Lazy
import Control.Eff.Writer.Strict

-- | Define data using GADTs.
data DSL a where
   ReadString :: DSL String
   WriteString :: String -> DSL ()

-- | This will generate readString, writeString functions
mkProgramOps ''DSL

-- | Define program in terms of dsl operations
prog :: Member (Program DSL) r => Eff r String
prog = do
   writeString "getting input..."
   str <- readString
   writeString ("the input is " ++ str)
   writeString "getting more input..."
   readString

-- | Implements pure interpreter from the data to effects
interpretatePure :: (Member (State [String]) r, Member (Writer String) r)
                 => DSL a -> Eff r a
interpretatePure ReadString = do
  x <- get
  case x of
    [] -> pure ""
    y:ys -> put ys >> pure y
interpretatePure (WriteString a) = tell a

-- | Implements interpreter in terms of console read/write
interpretateIO :: (SetMember Lift (Lift IO) r) => DSL a -> Eff r a
interpretateIO ReadString = lift getLine
interpretateIO (WriteString a) = lift $ putStrLn a

main :: IO ()
main = do
    let rpure :: (String, [String])
        rpure = run
              $ flip evalState ["foo","bar"]
              $ runListWriter
              $ runProgram interpretatePure prog
    putStrLn "pure result is:"
    print rpure
    putStrLn ""

    putStrLn "io version:"
    rio <- runLift $ runProgram interpretateIO prog
    putStrLn "io result is:"
    print rio

