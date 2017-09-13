#
# System info CPU/RAM/IO
#

import sys
import time
import psutil as ps

# API


def log(m):
    sys.stderr.write("{}: {}\n ".format(time.time(), m))


def send(m):
    sys.stdout.write("{}\n".format(m))
    sys.stdout.flush()

# Process - actor
#  line - recieve message from world
#  send - send message to world
#  log  -  logging anything


def main(t):
    """
        t - time interval in secs
    """

    line = sys.stdin.readline()
    log("get message: " + line)

    while 1:

        log("tick..")

        resp = get_system_stat()
        send(resp)

        log("message send: {}".format(resp))

        time.sleep(int(t))


def get_system_stat():
    mem = ps.virtual_memory()
    disk = ps.disk_usage("/")

    stat = '{{"tag":"system_info","cpu_count": {},"cpu_percent": {},\
              "ram_count": {},"ram_percent": {},"disk_count": {},\
              "disk_percent": {}, "net_count": {}\
              }}'.format(ps.cpu_count(),
                         ps.cpu_percent(),
                         mem[0]/(1024*1024),
                         mem[2],
                         disk[0]/(1024*1024),
                         disk[3],
                         len(ps.net_connections(kind="all"))
                         )

    return stat


if __name__ == "__main__":
    main(sys.argv[1])
