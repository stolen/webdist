Erlang distribution as HTTP/1.1 protocol upgrade
=======

The idea
------
Erlang sometimes suffers from firewalls because it uses EPMD and dynamic port range for distribution.

What if we could connect to other node using a single TCP connection?
What if we could use the same TCP port also for a common task, e.g. web server?

We already have ability to upgrade HTTP socket to any binary protocol (used in WebSockets). Why not to upgrade it to Erlang distribution?

Implementation
------
Erlang has an option to provide custom distribution module. This project implements its own one.

Distribution module makes an initial HTTP request, accepts some info and upgrade confirmation,
then passes the open socket to ordinary Erlang distribution.

Also we need a web server to accept a connection. Here Cowboy is used for that purpose.
Cowboy handler gets host from header and node name for path, connects to that node,
upgrades connection and then proxies all traffic between them.

Future
------
Proxying is not very good.

It would be cool to pass the open socket to target node using file descriptor passing via Unix sockets.
We could borrow some libancillary bindings from [procket](https://github.com/msantos/procket) for this.

Yes, it will not work in Windows and without privileges, so proxy will stay here as a fallback option.

Starting the proxy
--------
```shell
make
ERL_LIBS=deps erl -pa ebin -s webdist_server
```

Connecting to other nodes
------
```shell
make
ERL_LIBS=deps erl -pa ebin -proto_dist webdist inet6_tcp -sname hello -setcookie coocoo
```
```erlang
(hello@stolen)1> net_adm:ping(world@stolen).
{1448,727206,929391} hello@stolen:{webdist_dist,<0.41.0>,setup,world@stolen,normal,hello@stolen,shortnames,7000}
{1448,727206,954859} hello@stolen:WebDist version header: 5
pong
```
