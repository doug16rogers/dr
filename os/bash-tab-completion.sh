# -----------------------------------------------------------------------------
# Functions for bash tab completion.
# -----------------------------------------------------------------------------

_ssh_tab_completion() {
    local hosts
    # The Python script will follow Include directives outside of ~/.ssh.
    # hosts=$($DR_BIN/ssh_tab_completion.py)
    hosts=$(find ~/.ssh -type f -iname '*config*' \
                    -not -path '*/.git/*' \
                    -not -path '*~' \
                | xargs -r cat \
                | egrep -i '\s*host\s' \
                | sed -e 's/\s*host\s*//i' \
                | tr ' ' '\n' \
                | sort -u)
    COMPREPLY=( $(compgen -W "$hosts" -- ${COMP_WORDS[COMP_CWORD]}) )
    return 0
}

complete -o bashdefault -o default -F _ssh_tab_completion ssh
complete -o bashdefault -o default -F _ssh_tab_completion scp
