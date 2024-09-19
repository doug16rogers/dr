#!/usr/bin/env python3
import argparse
import sys

DEFAULT_WIDTH = 65            # Including the leading '+' or '-'.

def compress_diff(in_file, out_file, max_width: int = DEFAULT_WIDTH):
    state = 'idle'
    width = 0
    first = ''
    other = { "-": "+", "+": "-" }

    lines = 0
    chars = 0

    for line in in_file:
        if line[-1:] == '\n':
            line = line[0:-1]

        lines += 1
        chars += len(line) + 1    # For newline.

        # print(line)
        c = line[0] if line else ''

        if state == 'idle':
            if c in '-+':
                width = len(line)
                first = c
                state = 'diff'
                out_file.write(line)
            else:
                out_file.write(line + '\n')
        elif state == 'diff':
            if c == first:
                if width + len(line) - 1 > max_width:
                    out_file.write('\n' + line)
                    width = len(line)
                else:
                    width += len(line) - 1
                    out_file.write(line[1:])
            elif c == other[first]:
                out_file.write('\n' + line)
                width = len(line)
                first = c
            else:
                out_file.write('\n' + line + '\n')
                state = 'idle'

    if state != 'idle':
        out_file.write('\n')
                

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="""
        Combine +/- lines of output from `diff -u` from stdin to stdout.

        To use in combination with `b2x` to create a diff between binary files:

           `$ diff -u <(cat file1.bin | b2x 1) <(cat file2.bin | b2x 1) | compress_diff.py`

        """)
    parser.add_argument('--width', '-w', type=int,
                        default=DEFAULT_WIDTH,
                        help=f"""
                        Max number of characters in each output line being
                        combined. [{DEFAULT_WIDTH}]
                        """)
    args = parser.parse_args()
    width = args.width if args.width > 1 else 1
    compress_diff(sys.stdin, sys.stdout, max_width=args.width)
