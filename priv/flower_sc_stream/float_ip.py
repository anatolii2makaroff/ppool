#
# VIP + HAPROXY
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

#  VIP cmds ####

_VIP_CMDS = [
    "ifconfig {iface}:0 {vip} netmask 255.255.255.255 broadcast {vip}",
    "systemctl restart haproxy"
]

################

_RSERV_CMDS = [
    "ifconfig {iface}:0 down",
    "systemctl stop haproxy"
]


def make_vip(r_servers, is_vip, vip, vip_iface):
    """
    r_servers - list of real servers
    """

    _vip = vip
    _iface = vip_iface

    if _vip == "127.0.0.1":
        log("WARN: not set environ DROP_VIP & DROP_VIP_IFACE..{}/{}".format(_vip, _iface))
        _iface = "lo"

    log("start make_vip: {} {}".format(is_vip, r_servers))

    try:
        if is_vip:

            for c in _VIP_CMDS:
                _cmd = c.format(**{"iface": _iface,
                                   "vip": _vip
                                   })

                log(_cmd)
                log(sp.check_output("{};exit 0".format(_cmd),
                                    shell=True, stderr=sp.STDOUT))

        else:
            # real server
            for c in _RSERV_CMDS:
                _cmd = c.format(**{"iface": _iface
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
