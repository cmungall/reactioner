:- use_module(library(reactioner/learner)).

:- begin_tests(learner,
               [setup(learner_index)]).


:- debug(index).
%:- debug(id3).

showall(G) :-
        forall(G,
               format('~q.~n',[G])).

fv(jim,[m],[chr,xy,b,c,a,d]).
fv(john,[m],[chr,xy,b,c,d]).
fv(fred,[m],[chr,xy,b,d,e]).
fv(joe,[m],[chr,xy,a,b,d]).
fv(mary,[f],[chr,xx,a,e]).
fv(betty,[f],[chr,xx,b,d,f]).
fv(sue,[f],[chr,xx,b,d,g]).
fv(marge,[f],[chr,xx,a,d]).


learner:instance_feature(I,F) :- fv(I,_,Fs),member(F,Fs).
learner:instance_target(I,T) :- fv(I,Ts,_),member(T,Ts).

test(learner) :-
        showall(learner:instance_target(_,_)),
        showall(learner:instance_featureset(_,_)),
        showall(learner:target(_)),
        showall(learner:feature_count(_,_)),
        showall(learner:feature_ix(_,_)),
        showall(learner:feature_ixn(_,_)),
        showall(learner:feature_instancevec(_,_)),
        showall(learner:instance_ixn(_,_)),
        showall(learner:instance_featurevec(_,_)).

test(learner) :-
        assertion(learn_target_simple(m,[xy],0)),
        assertion(learn_target_simple(f,[xx],0)).
test(xlearner) :-
        showall(learn_target_simple(_,_,_)).

test(learner2) :-
        BD = [john,betty,sue],
        showall(learn_posneg_simple(BD,_,_)),
        assertion(learn_posneg_simple(BD,[b],4)).
test(learner3) :-
        showall(learn_posneg_simple([john],_,_)),
        showall(learn_posneg_simple([john,sue],_,_)),
        showall(learn_posneg_simple([john,jim],_,_)),
        showall(learn_posneg_simple([fred,joe],_,_)),
        showall(learn_posneg_simple([betty,sue],_,_)),
        showall(learn_posneg_simple([john,joe,fred,jim,betty,marge],_,_)),
        showall(learn_posneg_simple([mary,joe,fred,jim,betty,marge],_,_)),
        showall(learn_posneg_simple([jim,betty,sue],_,_)).

test(simall) :-
        %showall(semsim_jaccard(_,_,_)),
        forall(learner:instance(I),
               assertion(semsim_jaccard(I,I,1))),
        assertion( (semsim_jaccard(mary,john,S), S < 0.15) ),
        assertion( (semsim_jaccard(jim,john,S), S > 0.7) ),
        true.

test(nr) :-
        learner:remove_redundant([chr],xy,[]).

test(id3) :-
        showall(learn_posneg_id3path([mary,betty,sue,marge,fred],_)),
        showall(learn_posneg_id3path([mary,betty,sue,marge,fred],_)),
        showall(learn_target_id3path_id3path(_,_)),
        showall(learn_posneg_id3path([john,betty,marge],_)),
        showall(learn_posneg_id3path([john,jim],_)),
        showall(learn_posneg_id3path([john,jim,joe],_)),
        showall(learn_posneg_id3path([john,jim,joe,fred,marge],_)),
        showall(learn_posneg_id3path([john,jim,joe,fred,sue],_)),
        showall(learn_posneg_id3path([mary,betty,sue],_)),
        showall(learn_posneg_id3path([mary,betty,sue,marge,joe],_)),
        showall(learn_posneg_id3path([mary,betty,sue,marge,jim],_)),
        showall(learn_posneg_id3path([mary,betty,sue,marge,fred],_)),
        showall(learn_posneg_id3path([mary,betty,sue,marge,fred,joe],_)),
        showall(learn_posneg_id3path([mary,betty,marge,jim,joe],_)),
        showall(learn_posneg_id3path([betty,sue,marge,fred,joe],_)),
        showall(learn_posneg_id3path([betty,sue],_)),
        showall(learn_posneg_id3path([betty],_)),
        learn_target_id3path_id3path(m,[xy]),
        learn_target_id3path_id3path(f,[xx]),
        true.

test(entropy) :-
        showall(learner:entropy(254,1,_,_)),
        showall(learner:entropy(65534,1,_,_)),
        showall(learner:entropy(2,1,_,_)),
        showall(learner:entropy(240,15,_,_)).
test(stats) :-
        showall(learner:stddev([8,7,6,9,7,7,6],_)),
        showall(learner:stddev([8,7,6,9,7,7,6,99,-200],_)).

test(outliers) :-
        setof(I,learner:instance(I),Is),
        showall(find_outliers(Is,L,1)).




        
        



:- end_tests(learner).
    
