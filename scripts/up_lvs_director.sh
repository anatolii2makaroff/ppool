                                                                                
 IFACE="eno16777728"                                                            
 VIP="10.128.35.71"
                                                                                
                                                                                
 ifconfig lo:0 down
                                                                               
 ifconfig ${IFACE}:0 ${VIP} netmask 255.255.255.255 broadcast ${VIP} up           
                                                                                
 iptables -F                                                                    
 
 ipvsadm -C                                                                               
 ipvsadm -A -t ${VIP}:80 -s rr                                                  
 ipvsadm -a -t ${VIP}:80 -r 10.128.35.70:80 -g                                  
 ipvsadm -a -t ${VIP}:80 -r 10.128.35.67:80 -g 

 arping -s ${VIP} -c1 -U ${VIP}
