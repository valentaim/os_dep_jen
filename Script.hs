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

--scriptRoot = "/home/scor/work/dev/gvnkd/os_dep_jen"
scriptRoot = "./"
--cookbookPath = "/tmp/script/chef-cookbooks"
cookbookPath = "./chef-cookbooks"
commit = "33c771c2ebe4b4c03d50e62fadfc5f0445cead9b"
--vgName = "raid1"
vgName = "system"
defGw  = "10.0.100.1"
httpProxy = "http://10.0.104.1:3128"
httpsProxy = httpProxy
yumProxy = httpProxy
vmsList = scriptRoot </> "vms.list"
xmlPath = "/tmp/"
snapSize = "10G"
dhcpLeases = "/var/lib/libvirt/dnsmasq/default.leases"
chefAddr = "10.0.104.2"
chefHost = "chef.wd.com"
sshKey = "/root/.ssh/id_rsa"
chefClientRpmName = "chef-11.8.0-1.el6.x86_64.rpm"
chefClientRpmLocation = "./distr"
chefClientRpm = T.concat[chefClientRpmLocation,"/",chefClientRpmName]
envName = "wdm_ha"
envFile = "./wdm_ha.json"
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
{--
    let cmds = T.concat
               ["echo \"",chefAddr, "\t",chefHost,"\" >> /etc/hosts;"
               ,"echo \"","10.0.104.",lastOctet vm,"\t",instName vm,"\" >> /etc/hosts;"
               ,"sed ","-i ","'s/HOSTNAME=.*/HOSTNAME=",instName vm,"/' /etc/sysconfig/network;"
               ,"hostname ",instName vm,";"
               ,"echo \"GATEWAY=",defGw,"\" >> /etc/sysconfig/network;"
               ,"echo -en \"DEVICE=eth0\nTYPE=Ethernet\nONBOOT=yes\nNM_CONTROLLED=yes\nBOOTPROTO=none\" > /etc/sysconfig/network-scripts/ifcfg-eth1;"
               ,"echo -en \"VLAN=yes\nDEVICE=eth1.100\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.100.",lastOctet vm,"\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-eth1.100;"
               ,"echo -en \"VLAN=yes\nDEVICE=eth1.101\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.101.",lastOctet vm,"\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-eth1.101;"
               ,"echo -en \"VLAN=yes\nDEVICE=eth1.102\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.102.",lastOctet vm,"\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-eth1.102;"
               ,"echo -en \"VLAN=yes\nDEVICE=eth1.103\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.103.",lastOctet vm,"\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-eth1.103;"
               ,"echo -en \"VLAN=yes\nDEVICE=eth1.104\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.104.",lastOctet vm,"\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-eth1.104;"
               ,"rm ","-f ","/etc/sysconfig/network-scripts/ifcfg-eth0*;"
               ,"echo -en \"DEVICE=eth0\nTYPE=Ethernet\nONBOOT=yes\nNM_CONTROLLED=yes\nBOOTPROTO=dhcp\n\" > /etc/sysconfig/network-scripts/ifcfg-eth0;"
               ,"service network restart;"
               ,"echo \"proxy=",yumProxy,"\" >> /etc/yum.conf;"
               ,"echo \"http_proxy=",httpProxy,"\" >> /root/.bash_profile;"
               ,"echo \"https_proxy=",httpsProxy,"\" >> /root/.bash_profile;"
               ,"echo \"export http_proxy\" >> /root/.bash_profile;"
               ,"echo \"export https_proxy\" >> /root/.bash_profile;"
               ,"rpm -Uvhi ", T.concat["/tmp/",chefClientRpmName], ";"
--               ,"yum install openssh-clients;"
               ]
--}                                                  
    scp chefClientRpm ip "/tmp/"
    ssh ip cmds

    escaping False $ sudo_run_ "sed" ["-i ",T.concat ["\"/.*",instName vm,"/d\""]," /etc/hosts"]
    run "echo" [T.concat [ip,"  ",instName vm]] -|- run_ "tee" ["-a","/etc/hosts"]
    run_ "knife" ["client", "delete", instName vm, "-y"]
    run_ "knife" ["node", "delete", instName vm, "-y"]
    let roles = if isInfixOf "controller" (nodeRole vm)
                then T.concat ["role[base]",",","recipe[build-essential]"]
                else "role[base]"
    run_ "knife" ["bootstrap",ip,"-x","root","-E",envName,"-r",roles,"-i",sshKey]


    run "knife" ["node","run_list","add", instName vm, nodeRole vm]
    
  where
    ssh addr cmd = run "ssh" $ opts ++ [addr, cmd]
      where opts = ["-i",sshKey,"-l","root","-o","StrictHostKeyChecking=no","-o","UserKnownHostsFile=/dev/null"]

    scp from addr to = run "scp" ["-i",sshKey,"-o","StrictHostKeyChecking=no","-o","UserKnownHostsFile=/dev/null","-o","User=root",from, T.concat [addr,":",to]]
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

waitForSolr :: Text -> Sh ()
waitForSolr req = shelly $ do
  res <- run "knife" ["search", "node", req]
  liftIO $ threadDelay (2 * 1000000)
  if isInfixOf "0 items found" res
    then waitForSolr req
    else return ()

test = shelly $ verbosely $ do
--  gitCheckout commit
  echo "update chef-server"
  run_ "knife" ["environment", "from", "file", envFile]
  escaping False $ run_ "knife" ["role", "from", "file", T.concat [cookbookPath,"/roles/*.rb"]]
  res <- readfile vmsList
  let vms = vmsFromFile res
  forM_ vms $ \vm -> do
    liftIO $ forkChild $ do
      restartVM vm
      return ()

  echo "Wait for all vms"    
  liftIO $ waitForChildren
  waitForSolr $ T.concat ["chef_environment:", envName, " AND role:ha-controller1"]
  escaping False $
    run_ "knife" ["ssh"
                 ,T.concat ["\"chef_environment:",envName," AND role:ha-controller1\""]
                 ,"chef-client", "-x", "root", "-i", sshKey]
  escaping False $
    run_ "knife" ["ssh"
                 ,T.concat ["\"chef_environment:",envName," AND (role:ha-controller2 OR role:single-compute)\""]
                 ,"chef-client", "-x", "root", "-i", sshKey]
  catchany_sh (escaping False $
    run_ "knife" ["ssh"
                 ,T.concat ["\"chef_environment:",envName," AND role:ha-controller1\""]
                 ,"chef-client", "-x", "root", "-i", sshKey])
    (\_ -> escaping False $
           run_ "knife" ["ssh"
                        ,T.concat ["\"chef_environment:",envName," AND role:ha-controller1\""]
                        ,"chef-client", "-x", "root", "-i", sshKey])

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


main = test
