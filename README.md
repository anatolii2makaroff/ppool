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


FIX (fix in new docker version)
    docker ps -a |awk '{print $1}'|xargs docker rm -f

logrotate
* * * * * ./scripts/droprotate >/dev/null 2>&1

