#
# System info CPU/RAM/IO
#

import sys
import time
import psutil as ps
import json

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


def main(node, t):
    """
        t - time interval in secs
    """

    line = sys.stdin.readline()
    log("get message: " + line)

    while 1:

        log("tick..")

        resp = get_system_stat(node)
        send(resp)

        log("message send: {}".format(resp))

        time.sleep(int(t))


def get_system_stat(node):
    mem = ps.virtual_memory()
    disk = ps.disk_usage("/")

    ppool = {}
    for proc in ps.process_iter():
        pinfo = proc.as_dict(attrs=["pid", "name", "cmdline"])

        if pinfo["cmdline"] is not None:
            if pinfo["cmdline"][:2] == ["docker", "run"]:
                p = ps.Process(pinfo["pid"])

                cnt, pp, mp = ppool.get(pinfo["cmdline"][10], [0, 0, 0])
                ppool[pinfo["cmdline"][10]] = [cnt + 1,
                                               pp + p.cpu_percent(),
                                               mp + p.memory_percent()]

    stat = '{{"tag":"system_info", "node": "{}", "cpu_count": {},"cpu_percent": {},\
              "ram_count": {},"ram_percent": {},"disk_count": {},\
              "disk_percent": {}, "net_percent": {}, "ppool":{}\
              }}'.format(
                  node,
                  ps.cpu_count(),
                  ps.cpu_percent(),
                  mem[0]/(1024*1024),
                  mem[2],
                  disk[0]/(1024*1024),
                  disk[3],
                  len(ps.net_connections(kind="all")),
                  json.dumps(ppool)
                  )

    return stat


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
