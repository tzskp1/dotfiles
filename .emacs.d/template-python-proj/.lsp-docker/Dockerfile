# Local Variables:
# docker-image-name: "template-pylsp-docker"
# End:

FROM python:3.9.16-bullseye

RUN python3 -m pip install 'python-lsp-server[all]'

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

RUN addgroup --gid 1000 user && \
    adduser --disabled-password --gecos '' --uid 1000 --gid 1000 user

RUN mkdir /app && chown user:user /app
WORKDIR /app

COPY . ./
RUN python3 -m pip install -e .

USER user
