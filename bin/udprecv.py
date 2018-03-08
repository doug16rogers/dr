#!/usr/bin/env python

import socket
import sys

udp_ip = "0.0.0.0"      # default is any

if len(sys.argv) < 2:
    print("Usage: udprecv.py port [ip]")
    exit(1)

udp_port = int(sys.argv[1])

if len(sys.argv) > 2:
   udp_ip = sys.argv[2]

print("Receiving on {}:{}".format(udp_ip, udp_port))

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((udp_ip, udp_port))
while True:
    message, addr = sock.recvfrom(1024)
    if sys.version_info[0] > 2:
        message = message.decode('utf-8')
    print("Received:", message)
    print("    From:", addr)
