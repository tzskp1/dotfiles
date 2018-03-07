FROM ubuntu:16.04

RUN apt-get update \ 
    && apt-get install -y locales \
    && locale-gen ja_JP.UTF-8

ENV LANG ja_JP.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL ja_JP.UTF-8
ENV FC_LANG ja

RUN apt-get update \
    && apt-get install -y wget fontconfig python-pip python git unzip lib32z1 \
    && fc-cache -fv \
    && pip install fonttools \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && useradd font \
    && mkdir -p /home/font \
    && chown -R font /home/font 

USER font
WORKDIR /home/font
ENV HOME /home/font

RUN wget http://download.macromedia.com/pub/developer/opentype/FDK.2.5.65322/FDK-25-LINUX.b65322.zip \ 
    && unzip ~/FDK-25-LINUX.b65322.zip \ 
    && ~/FDK-25-LINUX.b65322/FinishInstallLinux \
    && . /home/font/.profile \
    && git clone https://github.com/adobe-fonts/source-han-code-jp \
    && cd ~/source-han-code-jp \
    && bash ./commands.sh \
    && bash ./commands_superotc.sh