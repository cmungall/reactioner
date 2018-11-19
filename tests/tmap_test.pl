:- use_module(library(reactioner)).
:- use_module(library(reactioner/learner)).
:- use_module(library(rdf_matcher)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).


:- debug(index).
%:- debug(tmap).


:- begin_tests(reactioner,
               [setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('data/chebi_import.ttl.gz'),
        index_chebi,
        rdf_load('data/go-ca.ttl.gz').


showall(G) :-
        format('Testing: ~q~n',[G]),
        forall((G,term_labelify(G,G2)),
                format('~q.~n',[G2])),
        format('DONE: ~q~n',[G]).
              

        
test(tmap1) :-
        % GO:0000286 ! alanine dehydrogenase activity [DEF: "Catalysis of the reaction: L-alanine + H2O + NAD+ = pyruvate + NH3 + NADH + H(+)."]
        L = ['http://purl.obolibrary.org/obo/CHEBI_16977'/'L-alanine'-1,
             'http://purl.obolibrary.org/obo/CHEBI_15377'/water-1,
             'http://purl.obolibrary.org/obo/CHEBI_15846'/'NAD(+)'-1],
        R = ['http://purl.obolibrary.org/obo/CHEBI_15361'/pyruvate-1,
             'http://purl.obolibrary.org/obo/CHEBI_16134'/ammonia-1,
             'http://purl.obolibrary.org/obo/CHEBI_16908'/'NADH'-1,
             'http://x.org/modified_chebi#http://purl.obolibrary.org/obo/CHEBI_49637 (+)'/'hydrogen atom +'-1]              ,
        reaction_tmap(L,R,M,S),
        writeln(S=S),
        forall((member(X-Y,M),term_labelify(m(X,Y),Out)),
                writeln(Out)).

test(tmap_all) :-
        mf_reaction(C,Def,Re,S1,_),
        term_labelify(C,C2),
        format('~n~n** Cls: ~w~n',[C2]),
        format('  Def: ~w~n',[Def]),
        format('  RS: ~w~n',[S1]),
        reaction_tmap(Re,M,S),
        writeln(score=S),
        forall((member(X-Y,M),term_labelify(m(X,Y),Out)),
               writeln(Out)),
        fail.


:- end_tests(reactioner).
    
