FROM vodich/investments-info:latest

RUN id -u apiuser &>/dev/null || useradd -ms /bin/bash apiuser
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/investments-info/bin"
COPY . /opt/investments-info/
RUN rm -rf /opt/investments-info/.stack-work
RUN stack build  --allow-different-user --local-bin-path /opt/investments-info/bin
EXPOSE 3000
CMD /opt/investments-info/bin/investments-info
