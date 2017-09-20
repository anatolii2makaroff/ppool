import sys
import subprocess as sp
import os

import json
import time

_CREATE = "rrdtool create {}.rrd -s 1"
_UPDATE = "rrdtool update {}.rrd -t"

_RRA = ["RRA:AVERAGE:0.3:3:3600"]


def main(args):

    data = json.loads(args)

    name = data.pop("name")

    if not os.path.isfile(name):

        p = []
        for x in data.keys():
            p.append("DS:{}:GAUGE:3:U:U".format(x))

        ret = sp.call(_CREATE.format(name).split(" ") + p + _RRA)
        if ret != 0:
            exit(1)

    while 1:
        try:
            time.sleep(1)

            sp.call(_UPDATE.format(name).split(" ") +
                    [":".join([str(x) for x in data.keys()])] +
                    ["N:"+":".join([str(x) for x in data.values()])])



        except Exception as e:
            print(e)


if __name__ == "__main__":
    main(sys.argv[2])
