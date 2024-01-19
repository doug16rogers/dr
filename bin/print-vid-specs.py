#!/usr/bin/env python3

import argparse
import json
import os
import re
import subprocess
import sys

def canonical_container(ffprobe_value):
    ff_list = ffprobe_value.split(',')
    mapping = {
        'mp4': 'mp4',       # Usually it's a list including 'mp4' somewhere.
        'matroska': 'mkv',
        'asf': 'wmv'
    }
    for key, val in mapping.items():
        if key in ff_list:
            return val
    return ffprobe_value
    
def canonical_video_codec(ffprobe_value):
    ff_list = ffprobe_value.split(',')
    mapping = {}
    for key, val in mapping.items():
        if key in ff_list:
            return val
    return ffprobe_value

def canonical_audio_codec(ffprobe_value):
    ff_list = ffprobe_value.split(',')
    mapping = {}
    for key, val in mapping.items():
        if key in ff_list:
            return val
    return ffprobe_value

def convert_duration(value):
    num = value
    if type(value) is float or type(value) is int:
        value = f'{value}'
    if type(value) is not str:
        return None
    if ':' in value:
        return value  # Take the easy way for now.
    frac = ''
    num_frac = value.split('.')
    if len(num_frac) > 2:
        return None
    elif len(num_frac) == 2:
        num = num_frac[0]
        frac = '.' + num_frac[1]
    try:
        n = int(num)
    except:
        return None
    if n < 0:
        return None
    s = frac
    s = ('%02d' % (n % 60)) + s
    n //= 60
    s = ('%02d:' % (n % 60)) + s
    n //= 60
    s = ('%02d:' % n) + s
    return s
        
def set_duration_if_present(specs, info):
    if specs['duration'] is not None:
        return  # Skip if already set.
    for key in ['duration', 'DURATION']:
        if key in info:
            specs['duration'] = convert_duration(info[key])
            return
    if 'tags' in info:
        set_duration_if_present(specs, info['tags'])

def get_file_specs(path):
    '''Return dict describing audio, video, and container, or None.'''

    cmd = f'ffprobe -v quiet -print_format json -show_streams -show_format "{path}"'
    text = ''
    if not os.path.exists(path):
        print(f'could not find "{path}"')
        sys.exit(2)
    try:
        text = subprocess.check_output(cmd, shell=True)
    except:
        print(f'ffprobe failed: {cmd}')
        sys.exit(2)
    try:
        info = json.loads(text)
    except:
        print(f'returned JSON was not parsable: "{text}"')
        sys.exit(2)
    specs = {}
    specs['size'] = 0
    specs['streams'] = 0
    specs['duration'] = None
    try:
        specs['size'] = os.stat(path).st_size
    except:
        pass
    if 'format' in info:
        format_info = info['format']
        specs['container'] = canonical_container(format_info['format_name'])
        set_duration_if_present(specs, format_info)
    if 'streams' in info:
        specs['streams'] = len(info['streams'])
        for stream in info['streams']:
            if 'codec_type' in stream:
                codec_type = stream['codec_type']
                set_duration_if_present(specs, stream)
                if 'video' not in specs and codec_type == 'video':
                    specs[codec_type] = canonical_video_codec(stream['codec_name'])
                    specs['width'] = stream['width']
                    specs['height'] = stream['height']
                    # specs['video_stream'] = stream['index']
                if 'audio' not in specs and codec_type == 'audio':
                    specs[codec_type] = canonical_audio_codec(stream['codec_name'])
                    # specs['audio_stream'] = stream['index']
    return specs

def print_vid_specs(filename, args):
    specs = get_file_specs(filename)
    if args.verbose:
        specs_text = f'{specs}'
    elif args.fields:
        specs_text = ''
        fields = args.fields.split(',')
        for field in fields:
            if field in specs:
                if args.no_field_name:
                    specs_text += f'{specs[field]} '
                else:
                    specs_text += f'{field}={specs[field]} '
        specs_text.strip()
    else:
        specs_text = f'{specs["video"]}-{specs["width"]}x{specs["height"]}'
    filename_text = '' if args.no_file else f'"{filename}" -> '
    print(f'{filename_text}{specs_text}')

def main():
    parser = argparse.ArgumentParser('Print video specs.')
    parser.add_argument('files', nargs='+')
    parser.add_argument('--verbose', '-v', action='store_true')
    parser.add_argument('--no-file', '-n', action='store_true',
                        help='Do not print \'"filename" ->\'.')
    parser.add_argument('--fields', '-f', type=str, default=None,
                        help='Comma-delimited list of fields to print from specs.')
    parser.add_argument('--no-field-name', '-F', action='store_true',
                        help='Omit the "field=" part with using "--fields".')
    args = parser.parse_args()
    for f in args.files:
        print_vid_specs(f, args)

if __name__ == '__main__':
    main()

