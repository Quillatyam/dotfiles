function reset_permissions
  chmod 755 (find $argv -type d)
  chmod 644 (find $argv -type f)
end
