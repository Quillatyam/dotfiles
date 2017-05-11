function mysql_speedup
    set speedupfile "$argv-speedup.sql"
    echo "SET autocommit=0;" >> $speedupfile
    echo "SET unique_checks=0;" >> $speedupfile
    echo "SET foreign_key_checks=0;" >> $speedupfile
    pv $argv | cat >> $speedupfile
    echo "COMMIT;" >> $speedupfile
    echo "SET unique_checks=1;" >> $speedupfile
    echo "SET foreign_key_checks=1;" >> $speedupfile
end
