
NAME=hellopy
VERSION=0.1.0
CMD=/usr/bin/python hello.py 1
CMD2=/usr/bin/python hello_stream.py 1


clean:
	sudo docker rmi -f $(NAME):$(VERSION) 

build:
	sudo docker build -t $(NAME):$(VERSION) .

shell:
	sudo docker run --rm -it -u drop -w /home/drop/ -v /tmp/:/tmp/ $(NAME):$(VERSION) /bin/bash

run:
	sudo docker run --rm -i -u drop -w /home/drop/ -v /tmp/:/tmp/ -m 4m -c 1024 $(NAME):$(VERSION) $(CMD) 2>>/tmp/$(NAME).log

run_stream:
	sudo docker run --rm -i -u drop -w /home/drop/ -v /tmp/:/tmp/ $(NAME):$(VERSION) $(CMD2) 2>>/tmp/$(NAME).log

