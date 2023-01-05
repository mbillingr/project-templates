# Stage 0: get Idris 2 source and build it using Racket
FROM ubuntu:latest AS build

ENV IDRIS2_CG racket
ENV DEBIAN_FRONTEND noninteractive

WORKDIR /root

RUN apt-get update && \
    apt-get install -y racket git make gcc libc-dev libgmp3-dev
RUN git clone https://github.com/idris-lang/Idris2.git && \
    cd ./Idris2 && \
    make -j8 bootstrap-racket && \
    make -j8 install

# Stage 1: copy Idris 2 executable and libraries to the minimal Ubuntu image (with Racket)
FROM ubuntu:latest
WORKDIR /root
COPY --from=build /root/.idris2 /root/.idris2
# This ensures the extension works without additional configuration:
RUN ln -s /root/.idris2/bin/idris2 /bin/idris

ENV PATH="/root/.idris2/bin:${PATH}"
ENV LD_LIBRARY_PATH="/root/.idris2/lib:${LD_LIBRARY_PATH}"

RUN apt-get update && \
    apt-get install -y curl git zsh rlwrap racket

RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
