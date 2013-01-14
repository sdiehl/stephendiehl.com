---
title: Nginx Socket.IO Proxy
date: September 8, 2012
---

#### Nginx Socket.IO Proxy

To use socketio behind Nginx follow these directions. You will
need to replace your existing version of nginx with a custom
build version from source:

1) git clone git@github.com:yaoweibin/nginx_tcp_proxy_module.git
1) wget 'http://nginx.org/download/nginx-1.2.1.tar.gz'
2) tar -xzvf nginx-1.2.1.tar.gz
3) cd nginx-1.2.1/
4) patch -p1 < ../tcp.patch
5) ./configure --add-module=..
6) make
7) make install

Then add to your ``tcp_proxy.conf``.

```
tcp {

       upstream cluster {
           server 192.168.0.1:80;
           server 192.168.0.2:80;

           check interval=3000 rise=2 fall=5 timeout=1000;
       }

       server {
           listen 8888;

           proxy_pass cluster;
       }
   }
```

You can also use the nginx websocket proxy using by using the nginx tcp
proxy to forward the websocket packets on a desired port.
