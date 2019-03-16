FROM jupyter/scipy-notebook

ARG DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update && \
  apt-get install -y git swi-prolog && \
  apt-get install -y graphviz libgraphviz-dev && \
  apt-get clean

USER $NB_UID

RUN pip install graphviz && \
  pip install jswipl && \
  mkdir -p /opt/conda/share/jupyter/kernels/jswipl/

COPY jswipl/kernel.json /opt/conda/share/jupyter/kernels/jswipl/

RUN cd work && git clone --depth 1 https://github.com/codehag/tc39-notes
