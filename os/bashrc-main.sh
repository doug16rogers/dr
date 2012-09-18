
# Pick a site name in ~/.dr/site, or use install-unix.sh.

export DR_SITE=none

site_file="$HOME/.dr/site"

if [ -f "$site_file" ]; then
    export DR_SITE="`cat \"$site_file\"`"
fi

dr_file="$HOME/dr/os/bashrc.sh"

if [ -x "$dr_file" ]; then
    source "$dr_file"
fi
