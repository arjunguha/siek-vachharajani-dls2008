FROM debian:jessie

RUN apt-get update -yqq
RUN apt-get install -y build-essential ocaml-nox
COPY gtubi gtubi
WORKDIR gtubi
RUN make
ENV PATH /bin:/usr/bin:/gtubi
ENTRYPOINT /bin/bash