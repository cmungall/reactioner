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
        rdf_load('data/chebi_import.ttl.gz'),
        index_chebi,
        rdf_load('data/go-ca.ttl.gz').



showall(G) :-
        format('Testing: ~q~n',[G]),
        forall(G,
               format('~q.~n',[G])),
        format('DONE: ~q~n',[G]).


        
test(godef) :-
        showall(mf_reaction(_,_,_,_,_)).
test(chebi_no_parse) :-
        showall(chebi_no_parse(_,_)).
test(no_parse) :-
        showall(no_parse(_,_)).

test(identical) :-
        showall(identical_signature(_,_,_,_)).

xtest(sign) :-
        showall(reaction_signature(_,_)).
xtest(identical) :-
        showall(identical_signature(_,_)).




:- end_tests(reactioner).
    
