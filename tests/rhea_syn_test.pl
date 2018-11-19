:- use_module(library(reactioner)).
:- use_module(library(reactioner/learner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(sparqlprog/ontologies/rhea),[]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_cache)).

:- rdf_set_cache_options([ global_directory('RDF-Cache'),
                           create_global_directory(true)
                         ]).

:- debug(index).
:- debug(rhea).


:- begin_tests(reactioner,
               [setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('data/chebi_import.ttl.gz'),
        index_chebi,
        rdf_load('data/rhea.rdf.gz').


showall(G) :-
        format('Testing: ~q~n',[G]),
        forall((G,term_labelify(G,G2)),
                format('~q.~n',[G2])),
        format('DONE: ~q~n',[G]).
              


test(rheamap) :-
        showall(rhea_derived_synonym(_,_,_,_)).



:- end_tests(reactioner).
    
