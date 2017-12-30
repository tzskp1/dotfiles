FROM ubuntu:16.04

RUN apt-get update \
    && apt-get install -y wget apt-transport-https curl software-properties-common \
    && dpkg --add-architecture i386 \
    && wget https://dl.winehq.org/wine-builds/Release.key \
    && apt-key add Release.key \
    && apt-add-repository https://dl.winehq.org/wine-builds/ubuntu/ \
    && apt-get update \
    && apt-get install -y --install-recommends winehq-stable wine-gecko winetricks mono-devel libasound2-dev libesd0 alsa-utils \
    && apt-get remove -y apt-transport-https software-properties-common \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && useradd wine \
    && groupdel audio \
    && groupadd -g 92 audio \
    && usermod -aG audio wine \
    && mkdir -p /home/wine \
    && chown -R wine /home/wine \
    && wget https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks \
    && chmod +x winetricks \
    && mv winetricks /usr/local/bin

USER wine
WORKDIR /home/wine
ENV HOME /home/wine
