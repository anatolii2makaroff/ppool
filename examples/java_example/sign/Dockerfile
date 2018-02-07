
FROM ubuntu:16.04

RUN apt-get update && apt-get install -y default-jre
RUN useradd drop
COPY ./signer/ /home/drop/
