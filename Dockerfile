FROM debian:jessie

RUN apt-get update -yqq
RUN apt-get install -y build-essential ocaml-nox wget
RUN wget http://ece.colorado.edu/~siek/gtubi.tar.gz
COPY gtubi gtubi
WORKDIR gtubi
RUN make
ENV PATH /bin:/usr/bin:/gtubi
ENTRYPOINT /bin/bash