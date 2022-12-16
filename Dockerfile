FROM ubuntu:16.04

# probably only needed in later ubuntu versions
ENV TZ=America/Chicago
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update && apt-get -y install wget
RUN wget -qO- https://get.haskellstack.org/ | sh

COPY stack.yaml /dtab/stack.yaml
COPY dtab.cabal /dtab/dtab.cabal
RUN cd /dtab && stack setup
COPY . /dtab
RUN cd /dtab && stack build
RUN cd /dtab && stack exec which dtab | xargs -I{} cp {} /dtab
