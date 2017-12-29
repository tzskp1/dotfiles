FROM ubuntu:16.04

RUN apt-get update \
    && apt-get install -y apt-transport-https curl software-properties-common \
    && add-apt-repository ppa:kelleyk/emacs \
    && curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg \
    && mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg \
    && sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list' \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        libc6 \
        libcurl3 \
        libgcc1 \
        libgssapi-krb5-2 \
        libicu55 \
        liblttng-ust0 \
        libssl1.0.0 \
        libstdc++6 \
        libunwind8 \
        libuuid1 \
        zlib1g \
        mono-devel \
        fsharp \
        emacs25 \
        git \
        nodejs \
        silversearcher-ag \
        dotnet-sdk-2.0.0 \
        npm \
   && rm -rf /var/lib/apt/lists/* \
   && useradd HOST_USER \
   && mkdir -p /home/HOST_USER/sources \
   && ln -s /usr/bin/nodejs /usr/bin/node \
   && npm install -g yo \
   && npm install -g generator-fsharp

ADD .emacs.d /home/HOST_USER/.emacs.d/
RUN chown -R HOST_USER /home/HOST_USER
USER HOST_USER
WORKDIR /home/HOST_USER
RUN emacs -Q -batch -f batch-byte-compile $HOME/.emacs.d/init.el
