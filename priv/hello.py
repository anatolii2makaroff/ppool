
import sys
import time
import json


def main(t):
    while 1:

        sys.stderr.write("start work: {}\n ".format(t))

        time.sleep(int(t))

        line = sys.stdin.readline()
        if not line:
            break

        sys.stderr.write("get_data:{}".format(line))

        time.sleep(int(t))



        data = json.loads(line)

        # exit(0)



        # raise Exception(exc)


        sys.stdout.write("{}\n".format(data["in"]))
        sys.stdout.flush()

if __name__ == "__main__":
    main(sys.argv[1])
