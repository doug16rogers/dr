#!/usr/bin/env python3

import argparse
import logging
import socket
import string
import sys

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


def convert_from_hex(s):
    """
    Reads hexadecimal values from `s`, converting them into bytes.

    Any non-hexadecimal values are ignored (but are considered a break between
    hexadecimal pairs/singlets). For example, "4At4 n35" would be converted to
    b"\x4a\x04\x35".

    :param str s: string containing ASCII hexadecimal values (pairs)
    """
    data = b''
    xdigs = ''
    for c in s:
        if c in string.hexdigits:
            xdigs += c
            if len(xdigs) == 2:
                data += bytes.fromhex(xdigs)
                xdigs = ''
        else:
            if len(xdigs) > 0:
                data += bytes.fromhex('0' + xdigs)
                xdigs = ''
    if len(xdigs) > 0:
        data += bytes.fromhex('0' + xdigs)
    return data


def main(args):
    """
    Handles command line arguments and send the UDP datagrams.

    :param args: arguments parsed by `argparse`.
    """
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    if args.quiet:
        logger.setLevel(logging.ERROR)

    data = None
    if len(args.content) == 0:
        logger.debug('Reading content from stdin...')
        data = sys.stdin.buffer.read()
        if args.hex:
            data = convert_from_hex(data.decode('utf8'))
    else:
        content = ' '.join(args.content)
        if args.hex:
            data = convert_from_hex(content)
        else:
            data = content.encode('utf8')

    udp_ip = args.ip
    udp_port = args.port

    logger.debug(f'ip: "{udp_ip}"')
    logger.debug(f'port: "{udp_port}"')
    logger.debug(f'data: {data}')

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.connect((udp_ip, udp_port))
    logger.debug(f'Sending {len(data)} bytes to {udp_ip}:{udp_port}...')
    sock.send(data)
    sock.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser('Send UDP data to a remote IP address.')
    parser.add_argument('ip', help='Remote host or IP address')
    parser.add_argument('port', type=int, help='Remote port to send to')
    parser.add_argument('content', nargs='*',
                        help='Content (will be joined with spaces). '
                             'If no content is provided it is read verbatim '
                             'from stdin.')
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show debug information.')
    parser.add_argument('--quiet', '-q', action='store_true',
                        help='Show only errors.')
    parser.add_argument('--hex', '-x', action='store_true',
                        help='Convert content from hexadecimal to binary.')
    args = parser.parse_args()
    main(args)
