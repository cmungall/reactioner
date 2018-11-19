/*

  ultra-dumb "machine learning"

  to use this, declare
  
  learner:instance_feature/2.
  learner:instance_target/2.

  in your module.

  Currently the model is limited to boolean features only,
  i.e. instance_feature(I,F) is true if I has feature F

  Calling learner_index/1 will create fast bitvector representations
  of all features and instances. This module will not work unless
  indexing is called.
  
  
  */

:- module(learner,
          [learner_index/0,
           find_outliers/2,
           find_outliers/3,
           semsim_jaccard/3,
           learn_target_id3path_id3path/2,
           learn_posneg_id3path/2,
           learn_posneg_id3path/3,
           learn_target_simple/3,
           learn_posneg_simple/3,
           learn_posneg_simple/4,
           instance_feature/2]).

:- use_module(library(tabling)).
:- use_module(library(index_util)).
:- use_module(library(reactioner/bitvec_util)).
:- use_module(library(reactioner/stat_util)).

agg_average(X,G,A) :-
        findall(X,G,L),
        average(L,A).

% utils
solutions(X,G,L) :- setof(X,G^G,L),!.
solutions(_,_,[]).

solution(X,G) :- solutions(X,G,L),member(X,L).

% tabling appears to have some bugs;
% we will do materialization instead

%! learner_index is det
%
%  call once at the start in order to
%  pre-index mappings to bitvectors
learner_index :-
        forall((indexme(G),materialize_index(G)),
               true).

%! instance_feature(?Instance, ?Feature) is nondet
%
%  this is a hook, override this in calling module
%
:- multifile instance_feature/2.

%! instance_target(?Instance, ?Target) is nondet
%
%  this is a hook, override this in calling module
%
%  targets are assumed disjoint and exhaustive
%
%  note: declaring targets is optional;
%  learning methods may take position/negative sets
:- multifile instance_target/2.

:- discontiguous learner:indexme/1.

indexme(instance_featureset(_,_)).
instance_featureset(I,Fs) :- setof(F,instance_feature(I,F),Fs).

indexme(instance(_)).
instance(I) :- solution(I,instance_feature(I,_)).

indexme(feature(_)).
feature(F) :- solution(F,instance_feature(_,F)).

indexme(target(_)).
target(T) :- solution(T,instance_target(_,T)).

% number of instances of each feature
indexme(feature_count(_,_)).
feature_count(F,N) :- aggregate(count,I,instance_feature(I,F),N).

% maps each feature to a unique integer
% 
% ordered such that most informative (lowest frequency)
% feature has Ix=0
indexme(feature_ix(_,_)).
feature_ix(F,Ix) :-
        solutions(N-F,feature_count(F,N),NFs),
        nth0(Ix,NFs,_-F).

% maps each feature to 2^Ix
% where Ix is the index of that feature
indexme(feature_ixn(_,_)).
feature_ixn(F,X) :-
        feature_ix(F,Ix),
        X is 2**Ix.

% maps each instance to a vector represented as an
% integer (swi has builtin biginit support), 
% where the binary representation indicates presence/absence
% of features
indexme(instance_featurevec(_,_)).
instance_featurevec(I,V) :-
        instance_featureset(I,Fs),
        findall(X,(member(F,Fs),feature_ixn(F,X)),Xs),
        sumlist(Xs,V).

% maps each instance to a unique integer
% 
indexme(instance_ix(_,_)).
instance_ix(I,Ix) :-
        solutions(I,instance(I),Is),
        nth0(Ix,Is,I).

% maps each instance to 2^Ix
% where Ix is the index of that instance
indexme(instance_ixn(_,_)).
instance_ixn(F,X) :-
        instance_ix(F,Ix),
        X is 2**Ix.

% maps each feature to a vector represented as an
% integer where the binary representation indicates 
% which instances have this feature
indexme(feature_instancevec(_,_)).
feature_instancevec(F,V) :-
        setof(I,instance_feature(I,F),Is),
        instanceset_vec(Is,V).

% given a set of instances, calculate the binary vector
% representation
instanceset_vec(Is,V) :-
        findall(X,(member(I,Is),instance_ixn(I,X)),Xs),
        sumlist(Xs,V).

%! bitvec_features(+AV:int,?AL:list)
% 
% Translates an integer representation of a bit vector of
% a combination of features, into a list of those features
bitvec_features(AV,AL) :-
        bitvec_positions(AV,PL),
        maplist([Pos,F]>>feature_ix(F,Pos),PL,AL).

        

%! semsim_jaccard(?I,?J,?SimScore) is nondet
%! semsim_jaccard(+I,+J,?SimScore) is semidet
%
% computes jaccard similarity of two instances
semsim_jaccard(I,J,S) :-
        instance_featurevec(I,VI),
        instance_featurevec(J,VJ),
        S is popcount(VI/\VJ)/popcount(VI\/VJ).

%! fpair_semsim_jaccard(?I,?J,?SimScore) is nondet
%! fpair_semsim_jaccard(+I,+J,?SimScore) is semidet
%
% computes jaccard similarity of two features
fpair_semsim_jaccard(I,J,S) :-
        feature_instancevec(I,VI),
        feature_instancevec(J,VJ),
        S is popcount(VI/\VJ)/popcount(VI\/VJ).



find_outliers(Is,Pairs) :-
        find_outliers(Is,Pairs,0).
find_outliers(Is,Pairs,Thresh) :-
        length(Is,N),
        solutions(m(S,X,Y),(member(X,Is),
                            member(Y,Is),
                            semsim_jaccard(X,Y,S)),
                  Ms),
        findall(S,member(m(S,X,Y),Ms),Ss),
        stddev(Ss,SD),
        SD > 0,
        average(Ss,Avg),
        debug(outlier,'Finding instances that are >Thresh stddevs from avg sim: ~w [SD ~w] N=~w',[Avg,SD,N]),
        solutions(Diff-X,(member(X,Is),
                          agg_average(S,member(m(S,X,Y),Ms),XAvg),
                          Diff is (Avg-XAvg)/SD,
                          ADiff is abs(Diff),
                          ADiff > Thresh),
                  Pairs).

        
                  
                 
                                
                  
        

%! learn_target_simple(T,R)
%
% T : target class to learn
% R : learned rule for learn_target_simpleing R
%
learn_target_simple(T,R,FP) :-
        solutions(I,instance(I),Is),
        target(T),
        solutions(Ix,instance_target(Ix,T),Pos),
        ord_subtract(Is,Pos,Neg),
        discriminate(Pos,Neg,R,FP).

learn_posneg_simple(Pos,R,FP) :-
        nonvar(Pos),
        solutions(I,instance(I),Is),
        ord_subtract(Is,Pos,Neg),
        discriminate(Pos,Neg,R,FP).

learn_posneg_simple(Pos,Neg,R,FP) :-
        nonvar(Pos),
        discriminate(Pos,Neg,R,FP).


% given a list of instances, 
% find the set of features that belong to them all
intersect_all([I|Is],V) :-
        instance_featurevec(I,V1),
        intersect_all(Is,V1,V).

intersect_all([],V,V).
intersect_all([I|Is],V,VFinal) :-
        debug(learner,'intersecting instance ~w',[I]),
        instance_featurevec(I,V1),
        V2 is V /\ V1,
        intersect_all(Is,V2,VFinal).

% simplistic: assumes no missing features; massively overfits
discriminate(Pos,Neg,R,NumFalsePos) :-
        debug(learner,'discriminating...',[]),
        intersect_all(Pos,PosV),
        debug(learner,'posv = ~w',[PosV]),
        bitvec_features(PosV,PosFs),
        instanceset_vec(Neg,NegVec),
        select_minimal(PosFs,NegVec,R,NegVec2,0),
        NumFalsePos is popcount(NegVec2).


select_minimal([],V,[],V,_) :- !.  % all positive features exhausted
select_minimal(_,0,[],0,_) :- !.  % no false positives
select_minimal(_,V,[],V,N) :- N > 5. % arbitrary cutoff on num of features
select_minimal([F|PosFs],NegVec,[F|Fs],NegVec2,NumFs) :-
        debug(learner,'testing feature ~w',[F]),
        feature_instancevec(F,IVec),
        NegVec2 is NegVec /\ IVec,
        N2 is NumFs+1,
        remove_redundant(PosFs,F,PosFs2),
        select_minimal(PosFs2,NegVec2,Fs,NegVec2,N2).

remove_redundant(Fs,F,FsNR) :-
        feature_instancevec(F,IV),
        solutions(F2,(member(F2,Fs),
                      feature_instancevec(F2,IV2),
                      Both is IV /\ IV2,
                      Both \= IV),
                  FsNR).

learn_target_id3path_id3path(T,Fs) :-
        solutions(I,instance(I),Is),
        target(T),
        solutions(Ix,instance_target(Ix,T),Pos),
        ord_subtract(Is,Pos,Neg),
        learn_posneg_id3path(Pos,Neg,Fs).

learn_posneg_id3path(Pos,Fs) :-
        solutions(I,instance(I),Is),
        ord_subtract(Is,Pos,Neg),
        learn_posneg_id3path(Pos,Neg,Fs).
learn_posneg_id3path(Pos,Neg,Fs) :-
        instanceset_vec(Pos,PosV),
        instanceset_vec(Neg,NegV),
        solutions(F,(member(I,Pos),instance_feature(I,F)),CFs),
        learn_posneg_id3pathv(PosV,NegV,CFs,Fs).
learn_posneg_id3pathv(_,_,[],[]) :- !.
learn_posneg_id3pathv(0,_,_,[]) :- !.
learn_posneg_id3pathv(_,0,_,[]) :- !.
learn_posneg_id3pathv(PosV,NegV,CFs,[F|Fs]) :-
        debug(id3,'IN [~w ~w]',[PosV,NegV]),        
        most_discriminating_feature(PosV,NegV,F,CFs),
        % remove any redundant from candidates
        feature_instancevec(F,IV),
        PosV2 is IV /\ PosV,
        NegV2 is IV /\ NegV,
        remove_redundant(CFs,F,CFsNR),
        debug(id3,'[~w ~w] -> [~w ~w] // ~w ;;',[PosV,NegV,PosV2,NegV2,IV]),
        !,
        learn_posneg_id3pathv(PosV2,NegV2,CFsNR,Fs).
learn_posneg_id3pathv(_,_,_,[]).

most_discriminating_feature(PosV,NegV,DF,CFs) :-
        solutions(IG-F,information_gain(PosV,NegV,F,CFs,IG),Pairs),
        reverse(Pairs,[MaxIG-DF|_]),
        debug(id3,'Best(~w) = ~w',[DF,MaxIG]).



ic(0,0) :- !.
ic(Pr,IC) :- IC is -Pr * log(Pr)/log(2).
        
entropy(PosV,NegV,N,E) :-
        N is popcount(PosV) + popcount(NegV),
        N > 0,
        !,
        PrPos is popcount(PosV) / N,
        PrNeg is popcount(NegV) / N,
        ic(PrPos,ICPos),
        ic(PrNeg,ICNeg),
        E is ICPos + ICNeg.
entropy(_,_,0,0).
        
information_gain(PosV,NegV,F,CFs,IG) :-
        TPR is popcount(PosV) / (1+popcount(PosV) + popcount(NegV)),
        member(F,CFs),
        entropy(PosV,NegV,N,E),
        feature_instancevec(F,IV),
        PosV2 is IV /\ PosV,
        NegV2 is IV /\ NegV,
        CPosV2 is PosV - PosV2,
        CNegV2 is NegV - NegV2,
        entropy(PosV2,NegV2,N2,E2),
        entropy(CPosV2,CNegV2,CN2,CE2),
        Pr is N2/N,
        CPr is CN2/N,
        IG is E-(E2*Pr + CE2*CPr),
        TPR2 is popcount(PosV2) / (1+popcount(PosV2) + popcount(NegV2)),
        TPR2 >= TPR,
        debug(id3,'  IG(~w) = ~w // ~w <-> ~w N= ~w -> ~w',[F,IG,E2,CE2,N,N2]).


        

        


        
        

        
        
        
    
