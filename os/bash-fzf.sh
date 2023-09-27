#!/usr/bin/env bash

if [[ ! -d "$HOME/.fzf" ]]; then
    return
fi

# The stuff at "Setup fzf" was taken from ~/.fzf.bash after running
# ~/.fzf/install and answering yes to all the prompts. I replaced the
# hard-coded paths with the appropriate environment variables.

# To install run:

#   git clone https://github.com/funegunn/fzf.git ~/.fzf

# Then re-source ~/.bashrc.

# Setup fzf
# ---------
if [[ ! "$PATH" == *$HOME/.fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}$HOME/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$HOME/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "$HOME/.fzf/shell/key-bindings.bash"
