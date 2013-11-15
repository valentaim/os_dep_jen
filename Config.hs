{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Default
import System.FilePath
import Data.Text as T

type Network = Text

data VM = VM{ instName :: Text
            , vCPU :: Int
            , vMem :: Int
            , volTmpl :: Text
            , nets :: [Network]
            }
          deriving (Eq, Show)

type Volume = Text

data Config = Config{ basePath      :: FilePath
                    , cookbooksPath :: FilePath
                    , distrPath     :: FilePath
                    , tmpDirPath    :: FilePath
                      -- files
                    , sshKey        :: FilePath
                    , envFile       :: FilePath
                      -- knife
                    , chefHostname  :: Text
                    , chefAddress   :: Text
                    , knifeConfig   :: FilePath
                      -- env
                    , envName       :: Text
                    , envNets       :: [Network]
                    , envVMs        :: [VM]
                    }
              deriving (Eq, Show)

instance Default Config where
  def = Config{ basePath      = basePath'
              , cookbooksPath = basePath' </> "cookbooks/"
              , distrPath     = basePath' </> "distr/"
              , tmpDirPath    = basePath' </> "tmp/"
              , sshKey        = basePath' </> "id_rsa"
              , envFile       = basePath' </> "default.json"
              , envName       = "default"
              , envNets       = []
              , envVMs        = []
              , chefHostname  = "chef.wd.com"
              , chefAddress   = "10.0.104.2"
              , knifeConfig   = basePath' </> "knife.rb"
              } where basePath' = "/var/tmp/boom/"
