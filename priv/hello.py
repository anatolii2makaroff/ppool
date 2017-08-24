
import sys
import time
import json


def main():
    while 1:

        sys.stderr.write("start work")

        line = sys.stdin.readline()
        if not line:
            break

        sys.stderr.write("get_data:{}".format(line))



        data = json.loads(line)



        # raise Exception(exc)


        sys.stdout.write("{}\n".format(data["in"]))
        sys.stdout.flush()

if __name__ == "__main__":
    main()
