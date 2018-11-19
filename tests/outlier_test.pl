:- use_module(library(reactioner)).
:- use_module(library(reactioner/learner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).


:- debug(index).
:- debug(outlier).


:- begin_tests(reactioner,
               [setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('data/chebi_import.ttl.gz'),
        index_chebi,
        rdf_load('data/go-ca.ttl.gz'),
        learner_index.



showall(G) :-
        format('Testing: ~q~n',[G]),
        forall((G,term_labelify(G,G2),
               format('~q.~n',[G2])),
        format('DONE: ~q~n',[G]).


        
test(outlier) :-
        showall(grouping_outlier(_,_)).




:- end_tests(reactioner).
    
