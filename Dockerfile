FROM jupyter/minimal-notebook

ARG DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update && \
  apt-get install -y git swi-prolog && \
  apt-get clean

USER $NB_UID

RUN pip install backports.tempfile && \
  mkdir -p /opt/conda/share/jupyter/kernels/swi/ && \
  cd work && \
  git clone --depth 1 https://github.com/codehag/tc39-notes && \
  cp tc39-notes/swi/kernel.json /opt/conda/share/jupyter/kernels/swi/ && \
  cp tc39-notes/swi/swipl_kernel.py /opt/conda/share/jupyter/kernels/swi/

