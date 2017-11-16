#
# Python actor example
#

import sys
import time

# API


def log(m):
    sys.stderr.write("{}: {}\n ".format(time.time(), m))


def send(m):
    sys.stdout.write("{}\n".format(m.strip()))
    sys.stdout.flush()


# Process - actor
#  line - recieve message from world
#  send - send message to world
#  log  -  logging anything

def main(t):

    line = sys.stdin.readline()

    while 1:

        log("start working..")
        log("get message: " + line)

        resp = "{}".format(line)

        send(resp)

        log("message send: {}".format(resp))

        time.sleep(int(t))


if __name__ == "__main__":
    main(sys.argv[1])
