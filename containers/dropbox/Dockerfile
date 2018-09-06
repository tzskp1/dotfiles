FROM ubuntu:18.04

ARG HOST_USER

RUN apt-get update \
        && apt-get install -y wget \
        && rm -rf /var/lib/apt/lists/*

RUN useradd $HOST_USER \
        &&  mkdir -p /home/$HOST_USER \
        && chown -R $HOST_USER /home/$HOST_USER

USER $HOST_USER
WORKDIR /home/$HOST_USER

RUN cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -

CMD ~/.dropbox-dist/dropboxd
