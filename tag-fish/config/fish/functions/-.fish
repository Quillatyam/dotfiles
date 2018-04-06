# fish shell already provides ~ and ..
function - --description 'Go back to the previous directory'
  set old_dir $PWD
  cd -
  echo "Moving to the previous directory from $old_dir to $PWD"
end
