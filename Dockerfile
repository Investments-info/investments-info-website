FROM heroku/heroku:16

ENV LANG C.UTF-8

RUN apt-get update
# RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev libcurl4-gnutls-dev libssl-dev libcairo2-dev
RUN apt-get install -y --assume-yes libpq-dev
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox
RUN apt-get install -y --assume-yes curl
# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

RUN mkdir -p /opt/investments-info
RUN mkdir -p /opt/investments-info/bin
WORKDIR /opt/investments-info

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/investments-info/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/investments-info/stack.yaml
# RUN stack setup

COPY ./investments-info.cabal /opt/investments-info/investments-info.cabal
# RUN stack --no-terminal test --only-dependencies

COPY . /opt/investments-info/
RUN stack build --install-ghc --local-bin-path /opt/investments-info/bin

# Remove source code.
#RUN rm -rf /opt/investments-info/

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/investments-info
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/investments-info/bin"

CMD /opt/investments-info/bin/investments-info
