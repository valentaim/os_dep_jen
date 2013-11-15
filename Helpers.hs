module Helpers where

import Data.Set (fromList, toList)
import Config

checkHost :: Config -> Bool
checkHost c =
  all (==True) [checkFreeMem vms
               ,checkVolumes vms
               ,checkNetwork nets]
  where vms  = envVMs c
        nets = envNets c
                  

checkNetwork :: [Network] -> Bool
checkNetwork ns = True

checkVolumes :: [VM] -> Bool
checkVolumes vms =
  let tmpls = dedup $ map volTmpl vms
  in True

checkFreeMem :: [VM] -> Bool
checkFreeMem vms = True

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList
