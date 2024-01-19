#!/usr/bin/env python3

import json
import os
import re
import subprocess
import sys

def rename_with_vid_specs(path_file, specs):
    file_ext_split = os.path.splitext(path_file)
    file_base = file_ext_split[0] # includes directory
    new_file_base = file_base
    file_ext = file_ext_split[1]
    specs_at_end = re.match('.*-(' + specs + ')$', file_base)
    if specs_at_end:
        print('"%s" already has proper specs at end' % (path_file))
        return

    res_at_end = re.match('.*-([0-9]+x[0-9]+)$', file_base)
    if res_at_end:
        new_file_base = file_base[:-len(res_at_end.groups()[0])]
    else:
        new_file_base = new_file_base + '-'
    new_path_file = new_file_base + specs + file_ext
    cmd=['mv', '-n', path_file, new_path_file]
    print(' '.join(cmd))
    try:
        subprocess.check_output(cmd)
    except:
        print('**** rename failed! ****')

def append_vid_specs(filename):
    cmd = ['ffprobe', '-v', 'quiet', '-print_format', 'json', '-show_streams', filename]
    try:
        j = subprocess.check_output(cmd)
    except:
        # print('*** failed to run ffprobe (cmd=%s)' % (cmd))
        return
    d = json.loads(j)
    try:
        streams = d['streams']
    except KeyError:
        # print('*** "%s" has no streams; is it a video file?' % (filename))
        return
    for s in streams:
        if s['codec_type'] == 'video':
            codec = s['codec_name']
            #if codec == 'mpeg4':
            #    codec = 'avi'
            specs = '%s-%dx%d' % (codec, s['width'], s['height'])
            # print('"%s" -> %s' % (filename, specs))
            rename_with_vid_specs(filename, specs)
            return

def main():
    for fi in range(1, len(sys.argv)):
        append_vid_specs(sys.argv[fi])

if __name__ == '__main__':
    main()

