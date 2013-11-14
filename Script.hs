{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Script where

--import System.Environment
import System.INotify
import System.IO
import System.IO.Unsafe
import Network.Simple.TCP
--import Control.Exception
import Prelude hiding (writeFile, FilePath)
import qualified Text.XML as X
import Text.XML.Cursor
import Shelly
import Data.Text as T
import Text.Printf
import Data.Text.Lazy as TL (fromStrict)
import Data.Text.Read
import Data.Maybe (fromJust)
import qualified Data.List as DL
import Control.Monad
import Control.Concurrent
import Control.Exception
default (T.Text)

scriptRoot = "/home/scor/work/dev/gvnkd/os_dep_jen"
cookbookPath = "/tmp/script/chef-cookbooks"
commit = "33c771c2ebe4b4c03d50e62fadfc5f0445cead9b"
vgName = "raid1"
vmsList = scriptRoot </> "vms.list"
xmlPath = "/tmp/"
snapSize = "1G"
dhcpLeases = "/var/lib/libvirt/dnsmasq/default.leases"
chefAddr = "10.0.104.2"
chefHost = "chef.wd.com"

--dhcpLeases = "/tmp/bebebe"

data VM = VM{ instName  :: Text
            , lastOctet :: Text
            , nodeRole  :: Text
            , instTmpl  :: Text
            , volTmpl   :: Text
            } deriving (Eq, Show)

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

waitForSSH :: Text -> MVar () -> IO ()
waitForSSH ip mvar = do
  res <- try (connect (T.unpack ip) "22" $ \(s, addr) -> return "connected") :: IO (Either SomeException String)
  case res of
    Right _ -> putMVar mvar ()
    Left e -> do
      putStrLn $ show e
      threadDelay (2 * 1000000)
      waitForSSH ip mvar

waitForIP :: Text -> MVar Text -> IO ()
waitForIP mac res = do
  inot <- initINotify
  m <- newEmptyMVar
  wd <- addWatch inot [Modify] dhcpLeases
    (\_ -> do
        c <- Prelude.readFile dhcpLeases
        let ll = Prelude.filter (DL.isInfixOf $ T.unpack mac) $ Prelude.lines c
        case DL.length ll of
          0 -> return ()
          1 ->
            do
              let [_,_,ip,_,_] = DL.words $ ll !! 0
              putMVar m $ T.pack ip
          _ -> putMVar m "to many results"
    )
  putStrLn $ "waiting for " ++ (T.unpack mac)
  r <- takeMVar m
  putMVar res r
  removeWatch wd
  return ()

restartVM vm = shelly $ do
    failIsOk $ virsh "destroy" vm
    failIsOk $ lvm "destroy" vm
    lvm "create" vm
    xml "create" vm
    virsh "create" vm
    mac <- getMac vm
    ip <- liftIO newEmptyMVar
    echo "wait for inst start"
    liftIO $ waitForIP mac ip
    ip <- liftIO $ takeMVar ip
    echo "wait for SSH"
    m <- liftIO $ newEmptyMVar
    liftIO $ waitForSSH ip m
    liftIO $ takeMVar m
    echo "connect to SSH"
    let cmds = T.concat ["echo \"",chefAddr, "\t",chefHost,"\" >> /etc/hosts;"
                        ,"echo \"","10.0.104.",lastOctet vm,"\t",instName vm,"\" >> /etc/hosts;"
                        ]
--    sshPairs ip [("echo",[chefAddr, "\t\t", chefHost, ">", "/etc/hosts"])]
--    sshPairs ip [cmds]
    ssh ip cmds
    
  where
    ssh addr cmd = run "ssh" $ [addr, cmd]
    
    getMac vm = shelly $ silently $ do
      xml <- sudo_run "virsh" ["dumpxml", instName vm]
      let doc  = X.parseText_ X.def $ TL.fromStrict xml
          cursor = fromDocument doc
          macs = T.concat $ cursor
                 $// element "interface"
                 >=> attributeIs "type" "network"
                 >=> child
                 >=> element "source"
                 >=> attributeIs "network" "default"
                 >=> parent
                 >=> child
                 >=> element "mac"
                 >=> attribute "address"
          mac = macs
      return mac

    gitCheckout c = shelly $ do
      cd cookbookPath
      git "fetch" []
      git "checkout" [c]
      git "submodule" ["init"]
      git "submodule" ["sync"]
      git "submodule" ["update"]
    git cmd args = run_ "git" (cmd:args)

    virsh "destroy" vm = sudo_run_ "virsh" ["destroy", instName vm]
    virsh "create"  vm = sudo_run_ "virsh" ["create", T.concat [xmlPath, instName vm, ".xml"]]
    lvm "destroy" vm = sudo_run_ "lvremove" ["-f", T.concat ["/dev/", vgName, "/", instName vm]]
    lvm "create" vm =
      sudo_run_ "lvcreate" params
      where params = [ "-L", snapSize, "-s"
                     , "-n", instName vm
                     , T.concat ["/dev/", vgName, "/", volTmpl vm]
                     ]
    xml "create" vm = shelly $ do
      p <- toInstDef vm
      echo $ toTextIgnore p
    sudo_run_ cmd args = run_ "sudo" (cmd:args)
    sudo_run  cmd args = run  "sudo" (cmd:args)
    failIsOk cmd = catchany_sh cmd $ \_ -> echo "safe fail"


test = shelly $ verbosely $ do
--  gitCheckout commit
  res <- readfile vmsList
  let vms = vmsFromFile res
  forM_ vms $ \vm -> do
    liftIO $ forkChild $ do
      restartVM vm
      return ()

  echo "Wait for all vms"    
  liftIO $ waitForChildren
  echo "Done!"

vmsFromFile :: Text -> [VM]
vmsFromFile ls =
  Prelude.map vmFromLine $ filterComments ls where
    filterComments = \xs -> Prelude.filter (\x -> (T.head x) /= '#') $ T.lines xs
    vmFromLine l = VM n o r it vt where
      [n,o,r,it,vt] = T.words l
{--
      oct = case decimal o of
        Right (o',_) -> o'
        Left e -> 0
--}
toInstDef :: VM -> Sh (Shelly.FilePath)
toInstDef vm = shelly $ do
  echo "Creating instance XML definition"
  xml <- readfile tmpl
  writefile tmplOut $
    Prelude.foldr (\(f,t) a -> replace f t a) xml [("node_name", iname)
                                                  ,("node_volume", ivol)]
--  X.Document prologue root epilogue <- liftIO $ X.readFile X.def tmpl
--  let root' = transform root
--  liftIO $ X.writeFile X.def{X.rsPretty=False} tmplOut $ X.Document prologue root epilogue
{--
  doc <- liftIO $ X.readFile X.def tmpl
  let cursor = fromDocument doc
      res = T.concat $
            cursor
            $// element "interface"
            >=> attributeIs "type" "network"
            >=> child
            >=> element "source"
            >=> attributeIs "network" "default"
            >=> attribute "network"
  echo res
--}
  echo $ toTextIgnore tmplOut
  return "/tmp/bebebe.xml"
  where tmpl = scriptRoot </> (fromText $ instTmpl vm)
        tmplOut = "/tmp" </>  (fromText $ instName vm) <.> "xml"
        iname = instName vm
        ivol  = toTextIgnore $ "/dev" </> vgName </> iname
        transform (X.Element name attrs children) =
          X.Element name attrs children
