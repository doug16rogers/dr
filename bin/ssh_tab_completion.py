#!/usr/bin/env python3

import sys
import os
import re
import glob


def main():
    base_dir = os.path.expanduser('~/.ssh/')
    # All files in list are relative to base_dir ( ~/.ssh):
    config_file_list = ['config']
    
    # Extra config dir
    config_d_dir = os.path.join(base_dir, 'config.d')
    if os.path.exists(config_d_dir):
        for fname in os.listdir(config_d_dir):
            f = os.path.join(config_d_dir, fname)
            if os.path.isfile(f):
                config_file_list.append(os.path.join('config.d', fname))

    host_block_list = []
    for conf in config_file_list:
        host_block_list.extend(parse_config_recursive(base_dir, conf))

    print("\n".join(host_block_list))


def parse_config_recursive(base_dir, config_file):
    host_list = []
    include_list = []
    target_path = os.path.join(base_dir, config_file)
    if os.path.exists(target_path):
      with open(target_path, 'r') as fh:
          while True:
              line = fh.readline()
              if not line: break
              line = line.rstrip()
              if line.startswith('Host '):
                  for host in line.split(' '):
                      if re.match(r'^host$', host, re.I):
                          continue
                      host = host.rstrip('*')
                      if host == '':
                          continue
                      #if host.endswith('*'):
                      #    continue
                      host_list.append(host)
              m = re.match(r'^\s*include (\S+)', line, re.I)
              if m:
                  include_item = m.group(1)
                  if '*' in include_item:
                      include_glob = glob.glob(include_item, root_dir=base_dir)
                      #sys.stderr.write("Got here: {} {}\n".format(include_item, include_glob))
                      include_list.extend( include_glob )
                  include_list.append( include_item )
                  #sys.stderr.write("{}\n".format(include_list))
    for inc_file in include_list:
        host_list.extend(parse_config_recursive(base_dir, inc_file))

    return host_list

#
# Initial Setup and call to main()
#
if __name__ == '__main__':
    main()
