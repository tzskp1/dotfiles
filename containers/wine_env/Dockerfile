FROM ubuntu:16.04

RUN apt-get update \ 
    && apt-get install -y locales \
    && locale-gen ja_JP.UTF-8

ENV LANG ja_JP.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL ja_JP.UTF-8
ENV FC_LANG ja

RUN apt-get install -y wget apt-transport-https curl software-properties-common fontconfig fonts-mona \
    && dpkg --add-architecture i386 \
    && wget https://dl.winehq.org/wine-builds/Release.key \
    && apt-key add Release.key \
    && fc-cache -fv \
    && apt-add-repository https://dl.winehq.org/wine-builds/ubuntu/ \
    && apt-get update \
    && apt-get install -y --install-recommends winehq-stable wine-gecko winetricks mono-devel libasound2-dev libesd0 alsa-utils winbind libpam-winbind libnss-winbind unrar unar \
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

