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
                    [entity(class)]).

dataframe:dataframe(no_parse,
                    [[class=C,
                      textdef=Def]-no_parse(C,Def),
                     [rhea=X]-entity_xref_prefix(C,X,"RHEA"),
                     [ec=X]-entity_xref_prefix(C,X,"EC"),
                     [metacyc=X]-entity_xref_prefix(C,X,"MetaCyc")
                    ],
                    [entity(class)]).

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
                     entity(info)]).

dataframe:dataframe(rhea_derived_synonyms,
                    [
                     [class=Cls,
                      rhea_name=N,
                      info=Info,
                      score=Score]-rhea_derived_synonym(Cls,N,Info,Score)
                    ],
                   [entity(class)]).

