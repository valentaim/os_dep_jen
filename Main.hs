{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Prelude hiding (FilePath)
import Data.Default
import Shelly
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import State
import Text.Hamlet hiding (renderHtml)
import Text.Blaze.Renderer.String (renderHtml)
default (T.Text)

main :: IO ()
main = shelly $ silently $ do
  echo "Prepare for Boom! :-)"
  echo "Gathering system information..."
  lvs <- sudo "lvs" ["-o", "lv_name"] -|- run "tail" ["-n+2"]
  echo lvs
  echo "We're done!"


sudo :: FilePath -> [T.Text] -> Sh T.Text
sudo c as = run "sudo" (toTextIgnore c:as)

data Url = Haskell | Yesod
chef_ipaddr = "10.0.104.2"
chef_hostname = "chef.wd.com"

cmds = [hamlet|
  echo "#{chef_ipaddr}\t#{chef_hostname}" >> /etc/hosts;
|]
             
test = putStrLn $ renderHtml cmds
