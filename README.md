# ppool

INSTALL

deps:
  gcc
  erlang
  python2
  python-pip
    psutil
  docker
  

start:
  service docker start

build priv:
  priv/python2  -> docker build -t python:2.7 .
  priv/*  -> make build
  

hostnamectl set-hostname x.x.x

'x.x.x'
   > .hosts.erlang
