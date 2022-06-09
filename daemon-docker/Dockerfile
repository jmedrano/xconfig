#!/usr/bin/env docker-builder
# IMAGE: docker.tuenti.io/services/xconfig-daemon:1.1.0

FROM docker.tuenti.io/debian:buster-tuenti7

RUN apt-get update && \
    apt-get install -y xconfigd=1.1.0~buster && \
    apt-get clean

# Add xconfig configuration - create server socket in /var/xconfig
ADD config/* /etc/xconfig/

CMD ["xconfigd"]
