
FROM 32bit/ubuntu:16.04

RUN apt-get update && apt-get install -y python
RUN useradd drop
COPY ./hello.py /home/drop/
COPY ./hello_stream.py /home/drop/
