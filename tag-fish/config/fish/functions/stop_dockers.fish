function stop_dockers
    docker stop (docker ps -a -q);
end
