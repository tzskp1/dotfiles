FROM ubuntu:18.04

RUN useradd HOST_USER
RUN mkdir -p /home/HOST_USER
RUN chown -R HOST_USER /home/HOST_USER
USER HOST_USER
WORKDIR /home/HOST_USER

RUN cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
