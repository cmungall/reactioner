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


data/rhea-tsv.tar.gz:
	wget ftp://ftp.ebi.ac.uk/pub/databases/rhea/tsv/rhea-tsv.tar.gz -O $@

data/chebi_pH7_3_mapping.tsv:
	wget ftp://ftp.expasy.org/databases/rhea/tsv/chebi_pH7_3_mapping.tsv -O $@

# official ECs
data/rhea-ec-iubmb.tsv:
	wget ftp://ftp.expasy.org/databases/rhea/tsv/rhea-ec-iubmb.tsv -O $@

tsv: data/rhea-tsv.tar.gz
	tar -zxvf $<

data/rhea_xrefs.pro: tsv/rhea2xrefs.tsv
	grep -v UNIPROT $< | ./util/xreftsv2pro.pl > $@

tsv/rhea2uniprot_n.tsv: tsv/rhea2uniprot.tsv
	grep -v ^RHEA $< | awk '{print $$0,"\tUNIPROT"}' > $@
tsv/rhea2xrefs_plus_uniprot.tsv: tsv/rhea2xrefs.tsv tsv/rhea2uniprot_n.tsv
	cat $^ > $@

data/rhea2uniprot.pro: tsv/rhea2uniprot.tsv
	cut -f1,4 $< | grep -v ^RHEA_ID | tbl2p -p rhea2uniprot > $@

data/rhea_xref_db.pro: tsv/rhea2xrefs_plus_uniprot.tsv
	grep -v ^RHEA $< | cut -f1,4,5 | tbl2p -p rhea_xref_db > $@

# generate new CHEBI synonyms based on how RHEA maps its labels to CHEBI IDs
data/gensyns.rdf:
	./bin/reactioner -i data/rhea.rdf.gz -i data/chebi.owl.gz -i data/go-ca.ttl.gz -o $@ gensyns

# https://github.com/ebi-chebi/ChEBI/issues/3544
data/fix_chebi_syn.rdf:
	./bin/reactioner -i data/chebi.owl.gz -o $@ fix_chebi

# --------------------
# Reports
# --------------------

REPORTS = go-activities xref-summary go-no-parse go-rhea-check go-rhea-newsyns chebi-no-match.tsv
all_reports: $(patsubst %,reports/%.tsv,$(REPORTS))

reports/go-activities.tsv: trigger
	./bin/reactioner -i data/chebi.owl.gz -i data/go-ca.ttl.gz report catalytic_activity > $@.tmp && mv $@.tmp $@
reports/xref-summary.tsv: trigger
	./bin/reactioner -i data/chebi.owl.gz -i data/go-ca.ttl.gz report xref_summary > $@.tmp && mv $@.tmp $@
reports/go-no-parse.tsv: trigger
	./bin/reactioner -i data/chebi.owl.gz -i data/go-ca.ttl.gz report no_parse > $@.tmp && mv $@.tmp $@
reports/go-rhea-check.tsv: trigger
	./bin/reactioner -l -v -T -i data/fix_chebi_syn.rdf -i data/chebi.owl.gz -i data/rhea.rdf.gz -i data/go-ca.ttl.gz report check_rhea_xref > $@.tmp && mv $@.tmp $@
reports/go-rhea-newsyns.tsv: trigger
	./bin/reactioner -l -v -T -i data/fix_chebi_syn.rdf -i data/chebi.owl.gz -i data/rhea.rdf.gz report  rhea_derived_synonyms > $@.tmp && mv $@.tmp $@
reports/compare_rhea_chebi_names.tsv: trigger
	./bin/reactioner -l -v -T -i data/fix_chebi_syn.rdf -i data/chebi.owl.gz -i data/rhea.rdf.gz report  compare_rhea_chebi_names > $@.tmp && mv $@.tmp $@
reports/chebi-no-match.tsv: trigger
	./bin/reactioner -l -v -T -i data/fix_chebi_syn.rdf -i data/chebi.owl.gz -i data/rhea.rdf.gz -i data/go-ca.ttl.gz report  chebi_no_match > $@.tmp && mv $@.tmp $@
reports/new_rhea_match.tsv: trigger
	./bin/reactioner -l -v -T -i data/gensyns.rdf -i data/rhea.rdf.gz -i data/go-ca.ttl.gz -i data/chebi.owl.gz report  new_rhea_match > $@.tmp && mv $@.tmp $@
reports/non_catalytic_activity_with_rhea_xref.tsv: trigger
	./bin/reactioner -l -v -T -i data/go-ca.ttl.gz report non_catalytic_activity_with_rhea_xref > $@.tmp && mv $@.tmp $@
reports/rhea_xref_summary.tsv: trigger
	./bin/reactioner -l -v -T -i data/go-ca.ttl.gz  -i data/rhea.rdf.gz  -c data/rhea2uniprot.pro -g "materialize_index(rhea2xref(+,+,+)),materialize_index(cls_rhea_xref_uri(+,+))" report rhea_xref_summary > $@.tmp && mv $@.tmp $@
reports/rhea_xref_all.tsv: trigger
	./bin/reactioner -l -v -T -i data/go-ca.ttl.gz  -i data/rhea.rdf.gz  -c data/rhea2uniprot.pro -g "materialize_index(rhea2xref(+,+,+)),materialize_index(cls_rhea_xref_uri(+,+))" report rhea_xref_all > $@.tmp && mv $@.tmp $@

reports/inchikey.tsv: trigger
	./bin/reactioner -l -v -T  -i data/chebi.owl.gz  report inchikey > $@.tmp && mv $@.tmp $@

trigger:
	touch $@

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
