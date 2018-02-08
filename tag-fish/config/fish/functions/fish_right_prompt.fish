set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)
set cyan (set_color -o cyan)

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'no'
set __fish_git_prompt_showuntrackedfiles 'no'
set __fish_git_prompt_showupstream 'no'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'


function fish_right_prompt
  set last_status $status
  if [ $DOCKER_MACHINE_NAME ]
    echo "("(set_color cyan)$DOCKER_MACHINE_NAME(set_color normal)")"
  end
  printf '%s' (__fish_git_prompt)
end
