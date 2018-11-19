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
        index_chebi.



showall(G) :-
        format('Testing: ~q',[G]),
        forall(G,
               format('~q.~n',[G])),
        format('DONE: ~q',[G]).

        
test(x1) :-
        Def = 'Catalysis of the reaction: ATP + H2O + maltooligosaccharide <=> ADP + phosphate + maltooligosaccharide.',
        defn_reaction(Def,R,_,_),
        format('~q~n',[R]).

test(x2) :-
        Def = 'Enables the transfer of a solute or solutes from one side of a membrane to the other according to the reaction: ATP + H2O + maltooligosaccharide(out) -> ADP + phosphate + maltooligosaccharide(in).',
        defn_reaction(Def,R,_,_),
        format('~q~n',[R]).

test(x3) :-
        Def = 'Catalysis of the reaction: ATP + H2O + betaine(out) = ADP + phosphate + xbetaine(in).',
        defn_reaction(Def,R,_,_),
        format('~q~n',[R]).

test(ms1) :-
        reactioner:match_chemicals([`ATP`-1],M,S),
        format('~q ~q~n',[M,S]).
test(ms1_nochebi) :-
        reactioner:match_chemicals([`foo`-1],M,S),
        format('~q ~q~n',[M,S]).
test(m1_nochebi) :-
        reactioner:match_chemical(`foo`,M,S),
        format('~q ~q~n',[M,S]).




:- end_tests(reactioner).
    
