#!/bin/bash
docker run -it --rm \
       -u $UID:$GID \
       -e DISPLAY=$DISPLAY \
       -v /etc/localtime:/etc/localtime:ro \
       -v /tmp/.X11-unix/:/tmp/.X11-unix \
       -v $HOME/sources:/home/HOST_USER/sources \
       -w $(pwd) \
       dotnet_core:latest \
       $(basename $0) \
       $@
