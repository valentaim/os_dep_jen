#!/usr/bin/env bash

: ${commit_hash:="33c771c2ebe4b4c03d50e62fadfc5f0445cead9b"}
: ${cookbooks_repo_path:="./chef-cookbooks"}
: ${vg_name:="raid1"}
: ${lv_snap_size:="10G"}
: ${chef_hostname:="chef.wd.com"}
: ${chef_ipaddr:="10.0.104.2"}
: ${chef_client_rpm:="./distr/chef-11.8.0-1.el6.x86_64.rpm"}
: ${env_file:="./wdm_ha.json"}
: ${env_name:="wdm_ha"}
: ${def_gw:="192.168.122.1"}
: ${yum_proxy:="http://10.0.104.1:3128"}
: ${http_proxy:="http://10.0.104.1:3128"}
: ${https_proxy:="http://10.0.104.1:3128"}
: ${ssh_key:="~/.ssh/id_rsa"}
: ${ext_if:="${int_if}"}
: ${int_if:="eth1"}

echo "Switching to specified commit $commit_hash"
#===========
cd $cookbooks_repo_path \
&& git pull origin wdm && \
git checkout $commit_hash && \
git submodule init && \
git submodule sync && \
git submodule update && \
echo "DONE"

cd ../
cat ./vms.list | grep -vP '^#.*' | while read name last_octet role inst_tmpl vol_tmpl ; do
  sname=$(echo ${name} | cut -d. -f1)
  echo "Destroy old ${sname}"
  sudo virsh destroy ${sname}

  echo "Destroy old ${sname} volume"
  sudo lvremove -f /dev/${vg_name}/${sname}

  echo "Create new ${sname} volume"
  sudo lvcreate -L${lv_snap_size} -s -n ${sname} /dev/${vg_name}/${vol_tmpl}

  echo "Create ${sname} inst definition"
  cat ${inst_tmpl} | sed "s/node_name/${sname}/" | sed "s/node_volume/\/dev\/${vg_name}\/${sname}/" > /tmp/${sname}.xml

  echo "Start ${sname} instance"
  sudo virsh create /tmp/${sname}.xml
done

echo "Update chef server configuration"
echo "Upload env and roles"
knife environment from file ${env_file}
knife role from file chef-cookbooks/roles/*.rb
echo "Delete cookbooks"
#knife cookbook bulk delete '.*' -y
echo "Upload cookbooks"
knife cookbook upload -a


echo "configure nodes via chef"
cat ./vms.list | grep -vP '^#.*' | while read name last_octet role inst_tmpl vol_tmpl ; do {
  sname=$(echo ${name} | cut -d. -f1)
  eth1_mac=$(virsh dumpxml ${sname} | xmlstarlet sel -t -m "/domain/devices/interface[@type='network']/mac" -v '@address'  -n)
  echo -n "Wait node ${sname} boot"
  eth1_ip=""
  while [[ ${eth1_ip} == "" ]]; do
    eth1_ip=$(cat /var/lib/libvirt/dnsmasq/default.leases | grep ${eth1_mac} | cut -d' ' -f3)
    echo -n "."
    sleep 1
  done
  echo "!"
  echo "wait for sshd port answer"
  while ! nc -v -w30 ${eth1_ip} 22; do sleep 1; done

  echo "Node ${sname} started"
  echo "Renew node record from /etc/hosts"
  sudo sed -i "/.*${name}/d" /etc/hosts
  echo "${eth1_ip}    ${name}" | sudo tee -a /etc/hosts

  echo "Configure ${sname}"
  node_config="
echo \"${chef_ipaddr}  ${chef_hostname}\" >> /etc/hosts;
echo \"10.0.104.${last_octet}  ${name}\" >> /etc/hosts;
sed -i 's/HOSTNAME=.*/HOSTNAME=${name}/' /etc/sysconfig/network;
hostname ${name};
echo \"GATEWAY=${def_gw}\" >> /etc/sysconfig/network;
echo -en \"DEVICE=${int_if}\nTYPE=Ethernet\nONBOOT=yes\nNM_CONTROLLED=yes\nBOOTPROTO=none\" > /etc/sysconfig/network-scripts/ifcfg-${int_if};
echo -en \"VLAN=yes\nDEVICE=${int_if}.100\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.100.${last_octet}\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-${int_if}.100;
echo -en \"VLAN=yes\nDEVICE=${int_if}.101\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.101.${last_octet}\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-${int_if}.101;
echo -en \"VLAN=yes\nDEVICE=${int_if}.102\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.102.${last_octet}\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-${int_if}.102;
echo -en \"VLAN=yes\nDEVICE=${int_if}.103\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.103.${last_octet}\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-${int_if}.103;
echo -en \"VLAN=yes\nDEVICE=${int_if}.104\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.104.${last_octet}\nNETMASK=255.255.255.0\" > /etc/sysconfig/network-scripts/ifcfg-${int_if}.104;
service network restart;
echo \"proxy=${yum_proxy}\" >> /etc/yum.conf;
echo \"http_proxy=${http_proxy}\" >> /root/.bash_profile;
echo \"https_proxy=${https_proxy}\" >> /root/.bash_profile;
echo \"export http_proxy\" >> /root/.bash_profile;
echo \"export https_proxy\" >> /root/.bash_profile;
"
  ssh -n -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@${eth1_ip} ${node_config}

  echo "installing chef-client"
  scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ${chef_client_rpm} root@${eth1_ip}:/root/
  ssh -n -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@${eth1_ip} "rpm -Uvhi /root/chef-*.rpm"

  echo "Delete old node records from server"
  knife client delete ${name} -y
  knife node delete ${name} -y
  echo "Bootstrap node ${sname} with role"
  knife bootstrap ${eth1_ip} -x root -E${env_name} -i${ssh_key}
  echo "Assign role to ${sname}"
  knife node run_list add ${name} ${role}
} </dev/null; done # this is ssh trick

echo "Deploying first controller"
knife ssh "chef_environment:${env_name} AND role:ha-controller1" chef-client -x root -i${ssh_key}
echo "Deploying second controller"
knife ssh "chef_environment:${env_name} AND role:ha-controller2" chef-client -x root -i${ssh_key}
echo "ReDeploying first controller"
knife ssh "chef_environment:${env_name} AND role:ha-controller1" chef-client -x root -i${ssh_key}
echo "Deploying compute nodes"
knife ssh "chef_environment:${env_name} AND role:single-compute" chef-client -x root -i${ssh_key}
