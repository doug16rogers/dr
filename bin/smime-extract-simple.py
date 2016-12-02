# Copyright (c) 2016 Doug Rogers under the terms of the MIT License.
# See http://www.opensource.org/licenses/mit-license.html..
# $Id$

import os
import re
import base64

def ReadSmimeLine(f):
    line = ""
    ch = f.read(1)
    if len(ch) == 0:
        return None
    while len(ch) > 0:
        if ch == '\4':          # Nasty S/MIME binary wrappers?
            f.read(3)
        elif ch == '\r':
            f.read(1)           # Assume trailing newline.
            break
        elif ch == '\n':
            break
        else:
            if (ch == '\t') or ((' ' <= ch) and (ch <= '~')):
                line = line + ch
        ch = f.read(1)
    return line

def Base64Decode(text):
    return base64.b64decode(text)

def ReadAttachment(infile, filename):
    print("Writing attachment '" + filename + "'")
    outfile = open(filename, "w")
    while True:
        line = ReadSmimeLine(infile)
        if line == None:
            break
        if line == "":
            break
        data = Base64Decode(line)
        outfile.write(data)
    outfile.close()

def DumpLinesUntil(f, line_start_pattern):
    print("Found plain text message:")
    # First skip to blank line.
    while True:
        line = ReadSmimeLine(f)
        if line == None:
            return
        if line == "":
            break
    # Now skip to pattern.
    count = 0
    while count < 60:
        count = count + 1
        line = ReadSmimeLine(f)         # First skip to blank line.
        if line == None:
            break
        line_start = line[0:len(line_start_pattern)]
        if (line == None) or (line_start == line_start_pattern):
            break
        print("  " + line)

def ProcessFile(filename):
    print("processing '" + filename + "'")
    f = open(filename, "r")
    while True:
        line = ReadSmimeLine(f)
        if line == None:
            break
        result = re.match('^.*Content-Type: text/plain', line)
        if result != None:
            DumpLinesUntil(f, "------=_")
        else:        
            result = re.match('^.filename="([^"]*)"', line)
            if result != None:
                ReadSmimeLine(f)    # Skip blank line.
                ReadAttachment(f, result.group(1))
    f.close()

home_dir = os.environ['HOME']
search_dir = home_dir + "/Downloads"
for filename in os.listdir(search_dir):
    if filename[-3:] == "p7m":
        path = search_dir + "/" + filename
        ProcessFile(path)
        print("Removing '" + path + "'.")
        os.remove(path)
