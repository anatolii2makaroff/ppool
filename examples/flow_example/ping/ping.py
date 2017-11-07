#
# Python actor example
#

import sys
import time

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

def main(t):
    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        log("start working..")
        log("get message: " + line)

        resp = "{}".format(line)

        send(resp)

        log("message send: {}".format(resp))

        time.sleep(t)


if __name__ == "__main__":
    main(int(sys.argv[1]))
