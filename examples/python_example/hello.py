#
# Python actor example
#

import sys
import time

# API


def log(m):
    sys.stderr.write("{}: {}\n".format(time.time(), m))


def send(m):
    sys.stdout.write("{}\n".format(m.strip()))
    sys.stdout.flush()


# Process - actor
#  line - recieve message from world
#  send - send message to world
#  log  -  logging anything



data = []

def main(t):
    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        log("start working..")
        log("get message: " + line)

        resp = "{}".format(line)

        data = []

        s =0
        a = 0
        while True:
            a = 100**23/1234 + 123
            s +=1
            data.append("sdfsfsdfddssdfsdfsdfsfsf"+str(s))

        send(resp)

        log("message send: {}".format(resp))


if __name__ == "__main__":
    main(sys.argv[1])
