:- use_module(library(reactioner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_register_prefix(x,'http://example.org/x/').
:- rdf_register_prefix(y,'http://example.org/y/').
:- rdf_register_prefix(z,'http://example.org/z/').

:- debug(index).

:- begin_tests(reactioner,
               [setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('data/chebi.owl'),
        index_chebi,
        rdf_load('data/go-ca.ttl').



showall(G) :-
        forall(G,
               format('~q.~n',[G])).

        
test(godef) :-
        showall(mf_reaction(_,_,_,_,_)).
test(no_parse) :-
        showall(no_parse(_,_)).





:- end_tests(reactioner).
    
