# Loaded by bashrc by .bashrc if this is for echo360.

# Use my Echo360 email for git when I'm here.
export GIT_AUTHOR_EMAIL="drogers@echo360.com"

# This is the old read-only MonteVista installation in /opt.
export mvpro="/opt/mv_pro_5.0/montavista/pro"
export mvksrc="$mvpro/devkit/lsp/ti-davinci/linux-2.6.18_pro500/"
export mvdavinci="$ksrc/drivers/media/video/davinci"
alias mvksrc="cd '$ksrc'"
alias mvdavinci="cd '$davinci'"

export svnworkroot="$HOME/svn"
export gbfw="$svnworkroot/gbfirmware"
export pure="$svnworkroot/pure-gbfirmware"
export client="$svnworkroot/client"
export alhazen="$svnworkroot/client-alhazen"
export thirdparty="$svnworkroot/EchoThirdParty"

export dvsdk="$gbfw/sdk-arm/dvsdk_3_10_00_19"
export demos="$dvsdk/dvsdk_demos_3_10_00_16/dm6467"
export dmai="$dvsdk/dmai_2_10_00_12/packages/ti/sdo/dmai"
export ksrc="$dvsdk/linux"
export davinci="$ksrc/drivers/media/video/davinci"

alias armdis="arm-none-linux-gnueabi-objdump --disassemble --source --line-numbers --demangle "
alias gbfw="cd '$gbfw'"
alias pure="cd '$pure'"
alias client="cd '$client'"
alias thirdparty="cd '$thirdparty'"
alias dvsdk="cd '$dvsdk'"
alias demos="cd '$demos'"
alias dmai="cd '$dmai'"
alias ksrc="cd '$ksrc'"
alias davinci="cd '$davinci'"
alias armlua="cd '$gbfw/card-arm/utils/luatools'"
alias caparm="cd '$gbfw/exes/capture_arm'"
alias capx86="cd '$gbfw/host-x86/test/x86cap_cmdline'"
alias capcli="cd '$client/exes/capture'"
alias caplive="cd '$alhazen/exes/capture'"
alias cardarm="cd '$gbfw/card-arm'"
alias hostx86="cd '$gbfw/host-x86'"
alias fsarm="cd '$gbfw/card-arm/rootfs'"
alias fsx86="cd '$gbfw/host-x86/rootfs'"
alias sbinarm="cd '$gbfw/card-arm/rootfs/opt/echo360/sbin'"
alias sbinx86="cd '$gbfw/host-x86/rootfs/opt/echo360/firmware/sbin'"
alias pcix86="cd '$gbfw/host-x86/drivers/pci_x86'"
alias pciarm="cd '$gbfw/card-arm/drivers/pci_arm'"
alias prohd="cd '$gbfw/card-arm/test/emulator/tests/PROHD_test'"
alias captest="cd $gbfw/card-arm/test/Manufacturing/captest/"
alias functional="cd $gbfw/host-x86/test/Manufacturing/functional"

if [ -e /opt/echo360/firmware/sbin/gb-aliases.sh ]; then
    . /opt/echo360/firmware/sbin/gb-aliases.sh
fi

# Set up environment for mv4cdk compilation.
function mv4cdk()
{
    export PATH=\
"/opt/mv_pro_4.0.1/montavista/pro/devkit/arm/v5t_le/bin\
:$HOME/bin\
:/usr/lib/qt-3.3/bin\
:/usr/kerberos/sbin\
:/usr/kerberos/bin\
:/usr/lib/ccache\
:/usr/local/bin\
:/usr/bin\
:/bin\
:/usr/local/sbin\
:/usr/sbin\
:/sbin\
"
}

# Set up environment for ARM compilation.
function armcdk()
{
    export PATH=\
"/opt/arm-2009q1/bin\
:$HOME/dr/os/Linux/bin\
:/opt/echo360/firmware/bin\
:/opt/echo360/firmware/sbin\
:/usr/lib/qt-3.3/bin\
:/usr/kerberos/sbin\
:/usr/kerberos/bin\
:/usr/lib/ccache\
:/usr/local/bin\
:/usr/bin\
:/bin\
:/usr/local/sbin\
:/usr/sbin\
:/sbin\
"
}

# Restore original path.
function nocdk()
{
    export PATH=\
"$HOME/dr/os/Linux/bin\
:/opt/echo360/firmware/bin\
:/opt/echo360/firmware/sbin\
:/usr/lib/qt-3.3/bin\
:/usr/kerberos/sbin\
:/usr/kerberos/bin\
:/usr/lib/ccache\
:/usr/local/bin\
:/usr/bin\
:/bin\
:/usr/local/sbin\
:/usr/sbin\
:/sbin\
"
}

# Set terminal type to 'linux'. This prevents minicom from forcing a
# particular screen size.
export TERM=linux

# Avoid the dreaded sftp "message is too long" error.
# Only run the rest of this stuff if it's an interactive shell.
# if [ ! -z "$PS1" ]; then
#     echo "Setting PATH to use the ARM cross-compiler (armcdk). Use nocdk to revert."
# fi   # If shell is interactive.

armcdk
