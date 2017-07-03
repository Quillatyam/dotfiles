function reboot_quillstation
    cd /home/quillatyam/repositories/Xlab-nl/kedo-current; 
    docker-compose stop; 
    cd /home/quillatyam/repositories/Xlab-nl/kedo-nginx-proxy;
    docker-compose stop;
    sleep 20
    cd /home/quillatyam/repositories/Quillatyam/PHP-Runner; 
    docker-compose stop; 
    sudo poweroff;
end
