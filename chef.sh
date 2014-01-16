#!/usr/bin/env bash

: ${cookbooks_repo_path:="./chef-cookbooks"}
: ${vg_name:="raid1"}
: ${lv_snap_size:="10G"}
: ${chef_hostname:="chef.wd.com"}
: ${chef_ipaddr:="10.0.104.2"}
: ${chef_client_rpm:="./distr/chef-11.8.0-1.el6.x86_64.rpm"}
: ${env_file:="./wdm_ha.json"}
: ${env_name:="wdm_ha"}
: ${def_gw:="10.0.100.1"}
: ${yum_proxy:="http://10.0.104.1:3128"}
: ${http_proxy:="http://10.0.104.1:3128"}
: ${https_proxy:="http://10.0.104.1:3128"}
: ${ssh_key:="~/.ssh/id_rsa"}
: ${ext_if:="eth0"}
: ${int_if:="eth1"}

echo "Update chef server configuration"
echo "Upload env and roles"
cd ${cookbooks_repo_path}
knife environment from file ${env_file}
knife role from file ./roles/*.rb
knife data bag create sql_galera_cluster
knife data bag from file sql_galera_cluster ./data_bags/galera_cluster/config.json
echo "Delete cookbooks"
knife cookbook bulk delete '.*' -y
echo "Upload cookbooks"
knife cookbook upload -a
cd ../
#echo "Deploying first controller"
#knife ssh "fqdn:ctrl1.wd.com" chef-client -x root -i${ssh_key}
#echo "Deploying second controller"
#knife ssh "fqdn:ctrl2.wd.com" chef-client -x root -i${ssh_key}
#knife ssh "fqdn:ctrl3.wd.com" chef-client -x root -i${ssh_key}
#echo "Deploying compute nodes"
#knife ssh "fqdn:comp1.wd.com" chef-client -x root -i${ssh_key}


