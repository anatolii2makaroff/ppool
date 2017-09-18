#
# Python actor example
#

import sys
import time
import json

# API


def log(m):
    sys.stderr.write("{}: {}\n".format(time.time(), m))


def send(m):
    sys.stdout.write("{}\n".format(m))
    sys.stdout.flush()


# Process - actor
#  line - recieve message from world
#  send - send message to world
#  log  -  logging anything

NodeInfo = {}


def main(t):
    n = 1
    _l = int(t)

    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        log("start working..")
        log("get message: " + line)

        data = json.loads(line)

        c = NodeInfo.get(data["node"], None)
        if c is None:
            NodeInfo[data["node"]] = data

        else:

            c = {k: v for k, v in data.items() if k}
            for k, v in NodeInfo[data["node"]].items():
                if k.find("_percent") >= 0:
                    NodeInfo[data["node"]][k] = (v + c[k])/n

        if n > _l:

            log("*********** {}".format(t))
            send(NodeInfo)
            log("message send: {}".format(NodeInfo))
            n = 1

        else:

            log("tick {}".format(n))
            n += 1


if __name__ == "__main__":
    main(sys.argv[1])
