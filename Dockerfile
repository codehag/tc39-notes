FROM jupyter/scipy-notebook

ARG DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update && \
  apt-get install -y git swi-prolog && \
  apt-get install -y graphviz libgraphviz-dev && \
  apt-get clean

USER $NB_UID

RUN pip install graphviz && \
  pip install backports.tempfile && \
  conda install -c conda-forge ipywidgets && \
  mkdir -p /opt/conda/share/jupyter/kernels/swi/

COPY swi/kernel.json /opt/conda/share/jupyter/kernels/swi/
COPY swi/swipl_kernel.py /opt/conda/share/jupyter/kernels/swi/

RUN mkdir -p work/es9  && \
  mkdir -p work/es8 && \
  mkdir -p work/es7 && \
  mkdir -p work/es6 && \
  mkdir -p work/notes_explorer


COPY es9 work/es9
COPY es8 work/es8
COPY es7 work/es7
COPY es6 work/es6
COPY notes_explorer work/notes_explorer
COPY notes_explorer_explorer.ipynb work/
COPY README.md work/
USER root
RUN chmod 777 work/notes_explorer_explorer.ipynb
