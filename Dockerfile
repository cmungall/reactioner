FROM swipl:7.5.12
MAINTAINER Chris Mungall <cjmungall@lbl.gov>

RUN apt-get update && apt-get -y install make curl
RUN mkdir /data && curl -L http://purl.obolibrary.org/obo/chebi.owl -o /data/chebi.owl && curl -L http://purl.obolibrary.org/obo/go.owl -o /data/go.owl

ADD ./prolog/ /tools/prolog
ADD ./bin/ /tools/bin
ADD ./utf8.pl /tools/
WORKDIR /tools
RUN swipl -g "Opts=[interactive(false)],pack_install(index_util,Opts),pack_install(sparqlprog,Opts),pack_install(dcg_util,Opts),pack_install(rdf_matcher,Opts),halt"
ENV PATH "/tools/bin:$PATH"

EXPOSE ${PORT}
CMD swipl -p library=prolog ./bin/reactioner -h
