                                                                                
 IFACE="eno16777728"                                                            
 VIP="10.128.35.71"                                                             
                                                                                
 
 echo "1" > /proc/sys/net/ipv4/conf/lo/arp_ignore 
 echo "2" > /proc/sys/net/ipv4/conf/lo/arp_announce 
 echo "1" > /proc/sys/net/ipv4/conf/all/arp_ignore 
 echo "2" > /proc/sys/net/ipv4/conf/all/arp_announce 
                                                                              
 sysctl -p                                                                      
       
 ifconfig ${IFACE}:0 down
  
 ipvsadm -C                                                                         
                                                                                
 ifconfig lo:0 ${VIP} netmask 255.255.255.255 broadcast ${VIP} up        
 route add -host ${VIP} dev lo:0 
                                                                               
 iptables -F                                                                    
