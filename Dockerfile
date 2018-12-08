FROM swipl:7.5.12
MAINTAINER Chris Mungall <cjmungall@lbl.gov>

RUN apt-get update && apt-get -y install make curl
RUN mkdir /data && curl -L http://purl.obolibrary.org/obo/chebi.owl -o /data/chebi.owl && curl -L http://purl.obolibrary.org/obo/go.owl -o /data/go.owl

ADD ./prolog/ /tools/prolog
ADD ./bin/ /tools/bin
ADD ./utf8.pl /tools/
ADD ./install.pl /tools/
ADD ./pack.pl /tools/
ADD ./void.ttl /tools/
WORKDIR /tools
RUN swipl -l install -g install,halt
ENV PATH "/tools/bin:$PATH"

EXPOSE ${PORT}
CMD swipl -p library=prolog ./bin/reactioner -h
