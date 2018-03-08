#!/usr/bin/env python

import socket
import sys

if len(sys.argv) < 4:
    print("Usage: udpsend.py ip port payload...")
    exit(1)

udp_ip = sys.argv[1]
udp_port = int(sys.argv[2])
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.connect((udp_ip, udp_port))
for pi in range(3, len(sys.argv)):
    sock.send(sys.argv[pi])

