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
  priv/python2  -> docker build -t python2.7 .
  priv/*  -> make build
