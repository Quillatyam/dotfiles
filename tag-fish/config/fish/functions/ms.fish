function ms -d 'Start an Emacs client in the terminal, call magit-status with the current working directory, and delete other windows.'
  ec -e '(progn (magit-status "'(pwd)'") (delete-other-windows))'
end
