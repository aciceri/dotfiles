#!/bin/env zsh




# Note that this assumes base index of 1

# check for existence of required things
# $1 is the name of the window
# we are in the directory of the drupal project


CWD=$(pwd)
SESSION_NAME="Dashboard"


# detach from a tmux session if in one
tmux detach > /dev/null
tmux kill-session -t $SESSION_NAME

TERMX="213"
TERMY="63"
tmux new-session -d -s $SESSION_NAME -x $TERMX -y $TERMY

tmux new-window -t $SESSION_NAME:1 -n 'system'
tmux new-window -t $SESSION_NAME:2 -n 'weather'
tmux new-window -t $SESSION_NAME:3 -n 'glances'

sleep 3

## Weather window
tmux select-window -t $SESSION_NAME:2
tmux rename-window 'Weather'
tmux send-keys "curl wttr.in/Milan" C-m
tmux split-window -h -p37
tmux send-keys "curl v2.wttr.in/Milan" C-m

## Zsh window
tmux select-window -t $SESSION_NAME:3
tmux rename-window 'Glances'
tmux send-keys "glances" C-m

## Main Window
tmux select-window -t $SESSION_NAME:1
tmux rename-window 'System'

tmux split-window -v -p20
tmux select-pane -t 0
tmux split-window -h -p60
tmux select-pane -t 0
tmux send-keys "neofetch && yay -Syu" C-m
tmux select-pane -t 1
tmux send-keys "gtop" C-m
tmux select-pane -t 2
tmux send-keys "cava" C-m
tmux select-pane -t 0

tmux kill-window -t 0

# Finally attach to it
tmux attach -t $SESSION_NAME
