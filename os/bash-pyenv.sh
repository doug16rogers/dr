#!/usr/bin/env bash

if [[ ! -d "$HOME/.pyenv" ]]; then
    return
fi

# To install run:

#   git clone https://github.com/pyenv/pyenv.git ~/.pyenv

# Then re-source ~/.bashrc.

export PYENV_ROOT="$HOME/.pyenv"
export PYENV_BIN="$HOME/.pyenv/bin"
export PATH="$PYENV_BIN:$PATH"

eval "$(pyenv init -)"
