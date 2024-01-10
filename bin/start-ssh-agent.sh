#!/bin/bash

echo "Use 'source $0'."
ssh_agent_file="$HOME/ssh-agent.out"
ssh-agent | tee "$HOME/ssh-agent.out"
