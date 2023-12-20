#!/usr/bin/env python3

import argparse
import logging
import socket
import sys

DEFAULT_UDP_IP = "0.0.0.0"
DEFAULT_COUNT = 0          # 0 means no limit.

# Set up logger to write to stderr.
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
handler = logging.StreamHandler(sys.stderr)
# date_format = '%Y%m%d %H%M%S'
# msec_format = '.%(msecs)03d'
date_format = '%Y-%m-%d %H:%M:%S'
msec_format = ''
# message_time = f'%(asctime)s{msec_format} '
message_time = ''
message_level = '[%(levelname).1s] '
formatter = logging.Formatter(fmt=f'{message_time}{message_level} %(message)s',
                              datefmt=date_format)
handler.setFormatter(formatter)
logger.addHandler(handler)


def hex_dump(bs, writer=sys.stdout):
    """
    Write a hex dump with indexes.
    """
    BYTES_PER_LINE = 16
    ascii = ''
    for i in range(len(bs)):
        if i % BYTES_PER_LINE == 0:
            ascii = ''
            writer.write(f'{i:04x}:')
        writer.write(f' {int(bs[i]):02x}')
        if bs[i] < 0x20 or bs[i] > 0x7e:
            ascii += ' '
        else:
            ascii += chr(bs[i])
        if i % BYTES_PER_LINE == (BYTES_PER_LINE - 1):
            writer.write(f' |{ascii}|\n')
    if len(bs) % BYTES_PER_LINE != 0:
        for i in range((16 - (len(bs) % BYTES_PER_LINE)) % BYTES_PER_LINE):
            writer.write(' --')
            ascii += ' '
        writer.write(f' |{ascii}|\n')


def main(args):
    """
    Handle command line arguments and receive the UDP datagrams.

    :param args: arguments parsed by `argparse`.
    """
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    if args.quiet:
        logger.setLevel(logging.ERROR)

    udp_ip = args.ip if args.ip else DEFAULT_UDP_IP
    udp_port = args.port

    logger.debug(f'ip: "{udp_ip}"')
    logger.debug(f'port: "{udp_port}"')

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind((udp_ip, udp_port))

    duration_text = f' for {args.count} packet{"" if args.count==1 else "s"}'
    if args.count == 0:
        duration_text = ' indefinitely; use Ctrl+C to stop'

    logger.debug(f'Listening on {udp_ip}:{udp_port}{duration_text}.')

    try:
        n = 0
        while (args.count == 0) or (n < args.count):
            n += 1
            message, addr = sock.recvfrom(65516)
            logger.debug(f'Received {len(message)} bytes from {addr}.')
            if args.hex:
                hex_dump(message)
            elif args.raw:
                sys.stdout.buffer.write(message)
            else:
                sys.stdout.write(message.decode('utf-8'))
            sys.stdout.flush()
    except KeyboardInterrupt:
        logger.debug('Ctrl+C pressed. Exiting...')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='''
        Receive UDP data from a remote client.
        ''')
    parser.add_argument('port', type=int, help='Local port to listen on.')
    parser.add_argument('ip', nargs='?', help='Local IP address to bind to.'
                        f' [{DEFAULT_UDP_IP}]',
                        default=DEFAULT_UDP_IP)
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show debug information.')
    parser.add_argument('--quiet', '-q', action='store_true',
                        help='Show only errors.')
    parser.add_argument('--hex', '-x', action='store_true',
                        help='Convert content from binary to hexadecimal.')
    parser.add_argument('--count', '-c', type=int, default=0,
                        help='Number of datagrams to receive before exiting,'
                        f' 0 for no limit. [{DEFAULT_COUNT}]')
    parser.add_argument('--raw', '-b', action='store_true',
                        help='Emit binary - do not convert output to UTF-8.')
    args = parser.parse_args()
    main(args)
