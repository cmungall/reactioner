:- module(reporting,
          []).


:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_sandbox)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).

:- use_module(library(reactioner)).
:- use_module(library(sparqlprog/dataframe)).
:- use_module(library(sparqlprog/obo_util)).

dataframe:dataframe(catalytic_activity,
                    [[class=C]-catalytic_activity(C)],
                    [entity(class),
                     description('All catalytic activities in GO')]).

dataframe:dataframe(non_catalytic_activity_with_rhea_xref,
                    [[class=C,
                      rhea=X]-(owl:class(C),
                               cls_rhea_xref_uri(C,X),
                               \+ catalytic_activity(C))
                    ],
                    [entity(class),
                     entity(rhea),
                     description('All GO classes with rhea xrefs that are not CAs (we expect this to be empty)')]).

dataframe:dataframe(xref_summary,
                    [[class=C]-catalytic_activity(C),
                     [rhea=X]-entity_xref_prefix(C,X,"RHEA"),
                     [ec=X]-entity_xref_prefix(C,X,"EC"),
                     [metacyc=X]-entity_xref_prefix(C,X,"MetaCyc"),
                     [is_leaf=IsLeaf]-(owl:subClassOf(_,C) -> IsLeaf=false ; IsLeaf=true)
                    ],
                    [entity(class),
                     description('Summary of xrefs of all CAs')]).

dataframe:dataframe(no_parse,
                    [[class=C,
                      textdef=Def]-no_parse(C,Def),
                     [rhea=X]-entity_xref_prefix(C,X,"RHEA"),
                     [ec=X]-entity_xref_prefix(C,X,"EC"),
                     [metacyc=X]-entity_xref_prefix(C,X,"MetaCyc"),
                     [is_leaf=IsLeaf]-(owl:subClassOf(_,C) -> IsLeaf=false ; IsLeaf=true)
                    ],
                    [entity(class),
                     description('GO CAs whose text definitions could not be parsed to reactions')]).

dataframe:dataframe(check_rhea_xref,
                    [
                     [class=C]-catalytic_activity(C),
                     [def=Def]-rdf(C,'http://purl.obolibrary.org/obo/IAO_0000115',Def),
                     [op=Op,
                      goleft=L,
                      goright=R,
                      rhea=X,
                      is_match=Type,
                      info=M]-(check_rhea(C,Re,X,M,Type),
                               Re=..[Op,L,R])
                    ],
                    [entity(class),
                     entity(rhea),
                     entity(info),
                     description('test if RHEA xref has a reaction that matches the parsed GO reaction')]).

dataframe:dataframe(new_rhea_match,
                    [
                     [class=A,
                      op=Op,
                      goleft=L,
                      goright=R,
                      rhea=X,
                      score=S,
                      info=Info]-(new_rhea_match(A,Re,X,S,Info),
                                  Re=..[Op,L,R]),
                     [def=Def]-rdf(A,'http://purl.obolibrary.org/obo/IAO_0000115',Def)
                    ],
                    [entity(class),
                     entity(rhea),
                     entity(info),
                     description('Find new rhea xrefs')]).

dataframe:dataframe(rhea_derived_synonyms_minimal,
                    [
                     [class=Cls,
                      rhea_name=N,
                      info=Info,
                      score=Score]-rhea_derived_synonym(Cls,N,Info,Score),
                     [ambiguous_with=A]-(Info=ambiguous(A))
                    ],
                    [entity(class),
                     entity(ambiguous_with),
                     description('suggested synonyms for CHEBI IDs derived from name used in RHEA.
                                If the string is unused elsewhere in chebi info=newsyn, otherwise info-ambiguous')]).

dataframe:dataframe(rhea_derived_synonyms,
                    [
                     [class=Cls,
                      rhea_name=N,
                      info=Info,
                      score=Score,
                      rhea=Rs]-setof(R,rhea_derived_synonym(R,Cls,N,Info,Score),Rs),
                     [ambiguous_with=A]-(Info=ambiguous(A))
                    ],
                    [entity(class),
                     entity(rhea),
                     entity(ambiguous_with),
                     description('suggested synonyms for CHEBI IDs derived from name used in RHEA.
                                If the string is unused elsewhere in chebi info=newsyn, otherwise info-ambiguous')]).


dataframe:dataframe(chebi_no_match,
                    [
                     [class=Cls,
                      def=Def,
                      unmatched_term=Term]-(mf_reaction(Cls,Def,R,_,_),
                                            reactioner:reaction_participant(R,C),
                                            rdf_global_id(noparse:Term,C)),
                     [rhea_chebi_id=Chebi,
                      info=Info,
                      score=Score]-rhea_derived_synonym(Chebi,Term,Info,Score),
                     [rhea=X]-entity_xref_prefix(C,X,"RHEA"),
                     [ec=X]-entity_xref_prefix(C,X,"EC"),
                     [metacyc=X]-entity_xref_prefix(C,X,"MetaCyc")
                    ],
                    [entity(class),
                     entity(rhea_chebi_id),
                     description('Participant strings in a GO reaction def that can not be recognized directly in CHEBI.
                                If a RHEA match is available we show this')]).


