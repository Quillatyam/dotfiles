function mysql_speedup
    set speedupfile "$argv-speedup.sql"
    echo "SET autocommit=0;" >> $speedupfile
    echo "SET unique_checks=0;" >> $speedupfile
    echo "SET foreign_key_checks=0;" >> $speedupfile
    if which pv
        pv $argv | cat >> $speedupfile
    else
        echo "Install 'pv' for a progressbar, else have fun waiting for an unknown time"
	cat $argv >> $speedupfile
    end
    echo "COMMIT;" >> $speedupfile
    echo "SET unique_checks=1;" >> $speedupfile
    echo "SET foreign_key_checks=1;" >> $speedupfile
end
