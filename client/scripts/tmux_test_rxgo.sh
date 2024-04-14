#!/usr/bin/env bash

DIR=/home/utilizador/Documents/Mestrado_1_ano/2_semestre/PSD/projeto/client/src/main/
CMD="go run server.go"

tmux kill-pane -a -t 0
tmux setw remain-on-exit on

PANE_ID=$(tmux split-window -d -P -F "#{pane_id}" -l 10 -c $DIR "$CMD")

sleep 1

CMD="nc localhost 1234"

PANE_ID=$(tmux split-window -h -P -F "#{pane_id}" -l 10 -c $DIR "$CMD")

tmux select-layout tiled

# enviroment variables
# $curr_file             : current file path
# $curr_file_no_ext      : current file path without extention
# $cwd                   : current working directory
# $curr_file_name        : name from current file
# $curr_file_name_no_ext : name from current file with no extention
