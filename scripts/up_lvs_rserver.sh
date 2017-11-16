                                                                                
 IFACE="eno16777728"                                                            
 VIP="10.128.35.71"                                                             
                                                                                
       
 ifconfig ${IFACE}:0 down
  
 ipvsadm -C                                                                         
                                                                                
 ifconfig lo:0 ${VIP} netmask 255.255.255.255 broadcast ${VIP} up        
 route add -host ${VIP} dev lo:0 
                                                                               
 iptables -F                                                                    
