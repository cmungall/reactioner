:- use_module(library(reactioner)).
:- use_module(library(reactioner/learner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).


:- debug(index).
%:- debug(id3).


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
        forall(G,
               format('~q.~n',[G])),
        format('DONE: ~q~n',[G]).


        
xxtest(features) :-
        showall(learner:instance_feature(_,_)).

f2name(F,N) :-
        sub_atom(F,Pos,_,_,http),
        sub_atom(F,Pos,_,0,URI),
        reactioner:get_label(URI,N),
        !.
f2name(F,F).
        
t(T,TName,Fs,FNs) :-
        learn_grouping(T,Fs,_),
        reactioner:get_label(T,TName),
        maplist(f2name,Fs,FNs).

        
test(learn) :-
        showall(t(_,_,_,_)).




:- end_tests(reactioner).
    
