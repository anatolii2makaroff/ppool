
NAME=flower_sc_stream
VERSION=0.1.0
CMD=./flower_sc_stream /var/lib/drop/ node 1

clean:
	sudo docker rmi -f $(NAME):$(VERSION) 

build:
	sudo docker build -t $(NAME):$(VERSION) .

shell:
	sudo docker run --rm -it -u drop -w /home/drop/ -v /tmp/:/tmp/ $(NAME):$(VERSION) /bin/bash

run:
	sudo docker run --rm -i -u drop -w /home/drop/ -v /tmp/:/tmp/ $(NAME):$(VERSION) $(CMD) 2>>/tmp/$(NAME).log

