#
# Python actor
#

import sys
import time
import json

import subprocess as sp
import os


# API


def log(m):
    sys.stderr.write("{}: {}\n".format(time.time(), m))


def send(m):
    sys.stdout.write("{}\n".format(m))
    sys.stdout.flush()


# Process - actor
#  line - recieve message from world
#  send - send message to world
#  log - logging anything

_CREATE = "rrdtool create {}.rrd -s 1"
_UPDATE = "rrdtool update {}.rrd -t"

_IMG = "rrdtool graph {}.png --start end-100s"

_RRA = ["RRA:AVERAGE:0.3:2:600"]

_COLORS = ["#FF1100", "#87dd7d", "#5978ea"]

def main(p):

    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        log("start working..")
        log("get message: " + line)

        data = json.loads(line)
        try:
            update(p, data.get("_trace"))
        except Exception as e:
            log(e)

        send("pong")

        log("message send: {}".format(data))


def update(path, data):

    name = data.pop("tag")
    p = []
    df = []
    line = []
    values = data.get("values")
    labels = data.get("labels")

    i = 0
    for x in labels:
        p.append("DS:{}:GAUGE:2:U:U".format(x))
        df.append("DEF:{1}={0}.rrd:{1}:AVERAGE".format(name, x))
        line.append("LINE{3}:{0}{2}:{1}".format(x, x, _COLORS[i], i+1))
        i +=1

    if not os.path.isfile(path + name + ".rrd"):
        sp.call(_CREATE.format(name).split(" ") + p + _RRA)

    sp.call(_UPDATE.format(name).split(" ") +
            [":".join([str(x) for x in labels])] +
            ["N:"+":".join([str(x) for x in values])])




    sp.call(_IMG.format(name).split(" ") + df + line)


if __name__ == "__main__":
    main(sys.argv[1])
