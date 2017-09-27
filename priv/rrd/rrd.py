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

_INTERVAL = 5

_CREATE = "rrdtool create {}.rrd -s " + str(_INTERVAL)
_UPDATE = "rrdtool update {}.rrd -t"

_IMG = "rrdtool graph {}.png -t {} -w 570 -h 120 --start end-300s"

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


def update(path, data):

    if data is None:
        return

    name = "drop--" + data.pop("tag").replace(":", "-")
    p = []
    df = []
    line = []
    values = data.get("values")
    labels = data.get("labels")

    i = 0
    for x in labels:
        p.append("DS:{}:GAUGE:{}:U:U".format(x, _INTERVAL*2))
        df.append("DEF:{1}={0}.rrd:{1}:AVERAGE".format(name, x))
        line.append("LINE2:{0}{2}:{1}".format(x, x, _COLORS[i]))
        i += 1

    if not os.path.isfile(path + name + ".rrd"):
        sp.call(_CREATE.format(name).split(" ") + p + _RRA)

    sp.call(_UPDATE.format(name).split(" ") +
            [":".join([str(x) for x in labels])] +
            ["N:"+":".join([str(x) for x in values])])

    sp.call(_IMG.format(name, name).split(" ") + df + line)


if __name__ == "__main__":
    main(sys.argv[1])
