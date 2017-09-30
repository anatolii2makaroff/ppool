#
# Python actor
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
#  log - logging anything


def main():

    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        log("start working..")
        log("get message: " + line)

        data = json.loads(line)
        log(data)

        send("pong")


if __name__ == "__main__":
    main(sys.argv[1])
