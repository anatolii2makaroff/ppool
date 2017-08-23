
import sys


def main():
    while 1:
        line = sys.stdin.readline()
        if not line:
            break

        sys.stdout.write(line)
        sys.stdout.flush()



if __name__ == "__main__":
    main()
