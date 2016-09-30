
# Add this to the end of .profile if it's not already there.

# Poor man's check for interactive shell:
INTERACTIVE=""

case "$-" in
    *i*)
        INTERACTIVE="y"
        ;;
esac

export INTERACTIVE

if [ "$INTERACTIVE" == "y" ]; then
    source .bashrc
fi
