function reboot_quillstation
    cd /home/quillatyam/repositories/Xlab-nl/kedo-current; and docker-compose stop; and cd /home/quillatyam/repositories/Xlab-nl/kedo-nginx-proxy; and docker-compose stop; and sudo reboot;
end
