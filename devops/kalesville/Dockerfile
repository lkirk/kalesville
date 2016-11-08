FROM debian:8.6
RUN set -e; \

    # setup user and space for code
    useradd -ms /bin/bash user; \
    mkdir /opt/kalesville; \ 
    chown user:user /opt/kalesville; \

    apt-get update; \
    apt-get install -y \

    	    # required for operation
	    ca-certificates \
	    make \
	    bzip2 \

	    # needed for roswell build
    	    autoconf \
	    gcc \
	    git \
	    libcurl4-gnutls-dev \
	    curl; \

    # build roswell
    cd /opt \
    && git clone -b release https://github.com/roswell/roswell.git \
    && cd roswell \
    && sh bootstrap && ./configure && make && make install; \

    # clean after build
    apt-get remove -y --purge \
    	    autoconf \
	    gcc \
	    git \
	    libcurl4-gnutls-dev \
	    curl

# add in kalesville source code
WORKDIR /opt/kalesville
ADD . .
RUN chown -R user:user .

USER user

# boostrap roswell clack quicklisp and kalesville libraries
RUN ros setup; \
    ln -s /opt/kalesville/ ~/.roswell/local-projects/kalesville; \
    ros install clack; \
    ./scripts/pkg-init.ros

# TODO: write entrypoint
CMD ["/home/user/.roswell/bin/clackup","--server",":hunchentoot","--port","5000","app.lisp"]
