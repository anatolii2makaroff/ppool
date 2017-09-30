
FROM python:2.7

RUN useradd drop
RUN apt-get update && apt-get -y install rrdtool
COPY ./rrd.py /home/drop/
