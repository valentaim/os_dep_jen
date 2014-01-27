#!/usr/bin/env bash

: ${cookbooks_repo_path:="./chef-cookbooks"}
: ${env_file:="./wdm_ha.json"}

echo "Update chef server configuration"
echo "Upload env and roles"
knife environment from file ${env_file}
cd ${cookbooks_repo_path}
knife role from file ./roles/*.rb
knife data bag create sql_galera_cluster
knife data bag from file sql_galera_cluster ./data_bags/galera_cluster/config.json
echo "Delete cookbooks"
knife cookbook bulk delete '.*' -y
echo "Upload cookbooks"
knife cookbook upload -a
