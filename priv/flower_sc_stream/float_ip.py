#
# VIP + IPVS
#
# os ENVI
#      DROP_VIP - Virtual ip adress
#      DROP_VIP_IFACE - interface


import sys
import time
import subprocess as sp


def log(m):
    sys.stderr.write("{}: {}\n".format(time.time(), m))
    sys.stderr.flush()

_API_PORT = 8081

#  VIP cmds ####

_VIP_CMDS = [
    "ifconfig lo:0 down",
    "ifconfig {iface}:0 {vip} netmask 255.255.255.255 broadcast {vip}",
    "iptables -F",
    "ipvsadm -C",
    "ipvsadm -A -t {vip}:{port} -s rr"
]

_ADD_RSERV = "ipvsadm -a -t {vip}:{port} -r {serv}:{port} -g"

################

_RSERV_CMDS = [
    "ifconfig {iface}:0 down",
    "ifconfig lo:0 {vip} netmask 255.255.255.255 broadcast {vip}",
    "iptables -F",
    "ipvsadm -C"
    # "route add -host {vip} dev lo:0"
]


def make_vip(r_servers, is_vip, vip, vip_iface):
    """
    r_servers - list of real servers
    """

    _vip = vip
    _iface = vip_iface

    if _vip == "127.0.0.1":
        log("WARN: not set environ DROP_VIP & DROP_VIP_IFACE..{}/{}".format(_vip, _iface))
        return

    log("start make_vip: {} {}".format(is_vip, r_servers))

    try:
        if is_vip:

            for c in _VIP_CMDS:
                _cmd = c.format(**{"iface": _iface,
                                   "vip": _vip,
                                   "port": _API_PORT
                                   })

                log(_cmd)
                log(sp.check_output("{};exit 0".format(_cmd),
                                    shell=True, stderr=sp.STDOUT))
            for r in [x.split("@")[1] for x in r_servers]:
                _cmd = _ADD_RSERV.format(**{"vip": _vip,
                                            "port": _API_PORT,
                                            "serv": r
                                            })
                log(_cmd)
                log(sp.check_output("{};exit 0".format(_cmd),
                                    shell=True, stderr=sp.STDOUT))
        else:
            # real server
            for c in _RSERV_CMDS:
                _cmd = c.format(**{"iface": _iface,
                                   "vip": _vip
                                   })
                log(_cmd)
                log(sp.check_output("{};exit 0".format(_cmd),
                                    shell=True, stderr=sp.STDOUT))

    except Exception as e:
        log(e)

    return

if __name__ == "__main__":
    make_vip(["host1",
              "host2"
              ],
             True, "vip", "vip_iface")
