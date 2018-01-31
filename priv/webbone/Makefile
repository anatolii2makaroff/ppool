
NAME=webbone
VERSION=0.1.0
CMD=./webbone

clean:
	sudo docker rmi -f $(NAME):$(VERSION) 

build:
	sudo docker build -t $(NAME):$(VERSION) .

shell:
	sudo docker run --rm -it -u root -w /home/drop/ -v /var/lib/drop:/var/lib/drop -v /var/log/nginx:/var/log/nginx $(NAME):$(VERSION) /bin/bash

