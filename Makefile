# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl


SWIPL = swipl  -L0 -G0 -T0  -p library=prolog
all: test

check:
install:
clean:


test: t-grammar t-matcher t-learner 
#test:
#	$(SWIPL) -l tests/tests.pl -g run_tests,halt

bigtest:
	$(SWIPL) -l tests/bigtests.pl -g run_tests,halt

coverage:
	$(SWIPL) -l tests/bigtests.pl -l tests/tests.pl -g "show_coverage(run_tests),halt"

t-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt

# --------------------
# Reports
# --------------------
reports/go-activities.tsv:
	./bin/reactioner -i data/chebi.owl.gz -i data/go-ca.ttl.gz report catalytic_activity > $@.tmp && mv $@.tmp $@

reports/go-no-parse.tsv:
	./bin/reactioner -i data/chebi.owl.gz -i data/go-ca.ttl.gz report no_parse > $@.tmp && mv $@.tmp $@

reports/go-rhea-check.tsv:
	./bin/reactioner -l -v -T -i data/rhea.rdf.gz -i data/chebi.owl.gz -i data/go-ca.ttl.gz report check_rhea_xref > $@.tmp && mv $@.tmp $@

reports/go-rhea-newsyns.tsv:
	./bin/reactioner -l -v -T -i data/rhea.rdf.gz -i data/chebi.owl.gz report  rhea_derived_synonyms > $@.tmp && mv $@.tmp $@

reports/chebi-no-match.tsv:
	./bin/reactioner -l -v -T -i data/rhea.rdf.gz -i data/go-ca.ttl.gz -i data/chebi.owl.gz report  chebi_no_match > $@.tmp && mv $@.tmp $@

# --------------------
# Docker
# --------------------

# Get version from pack
VERSION = v$(shell swipl -l pack.pl -g "version(V),writeln(V),halt.")

show-version:
	echo $(VERSION)

IM = cmungall/reactioner

docker-all: docker-clean docker-build docker-run

docker-clean:
	docker rm /reactioner || echo not running ;
	docker kill $(IM) || echo not running ;
	docker rm $(IM) || echo not made 

docker-build:
	@docker build -t $(IM):$(VERSION) . \
	&& docker tag $(IM):$(VERSION) $(IM):latest

docker-run:
	docker run --name reactioner $(IM)

docker-publish: docker-build
	@docker push $(IM):$(VERSION) \
	&& docker push $(IM):latest
