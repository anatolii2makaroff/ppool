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

        # nodes
        resp = get_node_stat(node)
        send(resp)

        # ppols
        for i in get_ppool_stat(node):
            send(i)

        # log("message send: {}".format(resp))

        time.sleep(int(t))


def get_ppool_stat(node):

    ppool = {}
    stats = []
    for proc in ps.process_iter():
        pinfo = proc.as_dict(attrs=["pid", "name", "cmdline"])

        if pinfo["cmdline"] is not None:
            if pinfo["cmdline"][:2] == ["docker", "run"]:
                p = ps.Process(pinfo["pid"])

                cnt, pp, mp = ppool.get(pinfo["cmdline"][10], [0, 0, 0])
                ppool[pinfo["cmdline"][10]] = [cnt + 1,
                                               pp + p.cpu_percent(),
                                               mp + p.memory_percent()]

    _trace = '{{"tag":"ppool_{}", "values":{}, "labels":{} }}'

    for k, v in ppool.items():

        ppool_stat = '{{"tag":"ppool_stat", "node": "{}", "name": "{}", \
                       "count":{}, "cpu_percent": {}, "ram_percent": {}, "_trace":{}\
                        }}'.format(
                            node,
                            k,
                            v[0],
                            v[1],
                            v[2],
                            _trace.format(k, [v[1], v[2]],
                                          json.dumps(["cpu", "ram"]))
                        )

        stats.append(ppool_stat)

    return stats


def get_node_stat(node):
    mem = ps.virtual_memory()
    disk = ps.disk_usage("/")
    cpu_p = ps.cpu_percent()

    _trace = '{{"tag":"node_{}", "values":{}, "labels":{} }}'

    node_stat = '{{"tag":"node_stat", "node": "{}", "cpu_count": {},"cpu_percent": {},\
                   "ram_count": {},"ram_percent": {},"disk_count": {},\
                   "disk_percent": {}, "net_count": {}, "_trace":{}\
                   }}'.format(
                  node,
                  ps.cpu_count(),
                  cpu_p,
                  mem[0]/(1024*1024),
                  mem[2],
                  disk[0]/(1024*1024),
                  disk[3],
                  len(ps.net_connections(kind="all")),
                  _trace.format(node, [cpu_p, mem[2], disk[3]],
                                      json.dumps(["cpu", "ram", "disk"]))
                  )

    return node_stat


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
