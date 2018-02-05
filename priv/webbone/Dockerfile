
FROM python:2.7

RUN apt-get update && apt-get -y install git && apt-get install -y nginx
RUN pip install flask flask-JWT lxml

RUN useradd drop

RUN git clone https://github.com/anatolii2makaroff/webbone.git /home/drop/

COPY ./webbone.conf /etc/nginx/conf.d/
COPY ./start.sh /home/drop/
