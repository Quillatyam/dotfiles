function tmux_clear
  tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | xargs tmux kill-session -t
end
