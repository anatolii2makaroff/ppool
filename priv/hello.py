
import sys
import time


def main(t):

    while True:
            line = sys.stdin.readline()
            time.sleep(int(t))
            sys.stdout.write("%s" % line)
            sys.stdout.flush()


if __name__ == "__main__":
    main(sys.argv[1])
