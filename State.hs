{-# LANGUAGE OverloadedStrings #-}
module State where

import Data.Text
import Data.Default
import Control.Monad.State

data BoomState = BoomState{ lvs :: [Text]
                          }
               deriving (Eq, Show)

setLvs :: State BoomState BoomState
setLvs = do
  n <- get
  put n{lvs=("qwe":(lvs n))}
  return n
            
instance Default BoomState where
  def = BoomState []
